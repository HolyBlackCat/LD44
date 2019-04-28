#include "config_data.h"
#include "image_data.h"

//{ Resources
constexpr ivec2 screen_size(480, 270);
Interface::Window window("Delta", screen_size * 2, Interface::windowed, ADJUST_G(Interface::WindowSettings{}, min_size = screen_size));
Graphics::DummyVertexArray dummy_vao;

Audio::Context audio_context;

const Graphics::ShaderConfig shader_config = Graphics::ShaderConfig::Core();
Interface::ImGuiController gui_controller(shader_config.common_header);

TextureAtlas texture_atlas;
Graphics::Texture texture_main = Graphics::Texture().Wrap(Graphics::clamp).Interpolation(Graphics::nearest);
Render r = ADJUST_G(Render(0x2000, shader_config), SetMatrix(fmat4::ortho(screen_size/ivec2(-2,2), screen_size/ivec2(2,-2), -1, 1)));
AdaptiveViewport adaptive_viewport(shader_config, screen_size);

Input::Mouse mouse;

Images images;

Random random;

Graphics::Font font;
//}

namespace Keys
{
    Input::Button left   = Input::left;
    Input::Button right  = Input::right;
    Input::Button jump   = Input::x;
    Input::Button attack = Input::z;
}

namespace Sounds
{
    #define SOUND_LIST(x) \
        x( sword_attack   , 0.4 ) \
        x( death          , 0.2 ) \
        x( death_speaks   , 0.3 ) \
        x( dialogue_click , 0.3 ) \

    #define X(name, random_pitch) \
        Audio::Buffer _buffer_##name(Audio::Sound(Audio::wav, Audio::mono, "assets/sounds/" #name ".wav")); \
        Audio::Source name(fvec2 pos, float vol = 1, float pitch = 1)                                       \
        {                                                                                                   \
            pitch = pow(2, std::log2(pitch) + float(-random_pitch <= random.real() <= random_pitch));       \
            return Audio::Source(_buffer_##name).temporary().volume(vol).pitch(pitch).pos(pos);             \
        }                                                                                                   \
        Audio::Source name(float vol = 0, float pitch = 0)                                                  \
        {                                                                                                   \
            return name(fvec2(0), vol, pitch).relative();                                                   \
        }                                                                                                   \

    SOUND_LIST(X)
    #undef X

    #undef SOUND_LIST
}

namespace Tiles
{
    enum RenderMode
    {
        simple,
        merged,
    };

    enum Category
    {
        non_solid,
        solid,
        kills,
    };

    struct Info
    {
        RenderMode render_mode;
        ivec2 tex_pos = ivec2(0);
        Category category = solid;
    };

    std::vector<Info> info_list
    {
        /* 0,0 - wall  */ {merged, ivec2(0,0), solid},
        /* 1,0 - spike */ {simple, ivec2(1,0), kills},
    };

    const Info &GetInfo(int index)
    {
        if (index == 0)
            Program::Error("Attempt to get tile info for a null tile.");
        index--;
        if (index < 0 || index >= int(info_list.size()))
            Program::Error("Invalid tile index: ", index, ".");

        return info_list[index];
    }
};

class Map
{
  public:
    static constexpr int tile_size = 12;

    Tiled::TileLayer layer_mid;
    Tiled::PointLayer points;

    Map() {}
    Map(std::string name)
    {
        try
        {
            Json json(MemoryFile("assets/maps/{}.json"_format(name)).string(), 64);

            layer_mid = Tiled::LoadTileLayer(Tiled::FindLayer(json.GetView(), "mid"));

            for (ivec2 pos : vector_range(layer_mid.size()))
            {
                int tile_index = layer_mid.try_get(pos);
                if (tile_index == 0)
                    continue;
                if (tile_index < 0 || tile_index > int(Tiles::info_list.size()))
                    Program::Error("Invalid tile index ", tile_index, " at position ", pos, ".");
            }

            points = Tiled::LoadPointLayer(Tiled::FindLayer(json.GetView(), "points"));
        }
        catch (std::exception &e)
        {
            Program::Error("While loading map `", name, "`: ", e.what());
        }
    }

    void Render(ivec2 cam_pos, ivec2 viewport_size = screen_size) const
    {
        ivec2 a = (cam_pos - viewport_size/2) / tile_size;
        ivec2 b = (cam_pos + viewport_size/2) / tile_size;

        for (ivec2 tile_pos : a <= vector_range <= b)
        {
            int tile_index = layer_mid.try_get(tile_pos);
            if (tile_index == 0)
                continue;

            ivec2 pixel_pos = tile_pos * tile_size - cam_pos;

            const auto &info = Tiles::GetInfo(tile_index);

            switch (info.render_mode)
            {
              case Tiles::simple:
                {
                    r.iquad(pixel_pos, images.tiles.Region(info.tex_pos * tile_size, ivec2(tile_size)));
                }
                break;
              case Tiles::merged:
                {
                    int depth = 0;

                    bool same_adjacent = 1;
                    for (int i = 0; i < 4; i++)
                    {
                        if (tile_index != layer_mid.try_get(tile_pos + ivec2(1,0).rot90(i)) ||
                            tile_index != layer_mid.try_get(tile_pos + ivec2(1,1).rot90(i)))
                        {
                            same_adjacent = 0;
                            break;
                        }
                    }

                    if (same_adjacent)
                    {
                        depth++;

                        for (int i = 0; i < 4; i++)
                        {
                            if (tile_index != layer_mid.try_get(tile_pos + ivec2(2,0).rot90(i)) ||
                                tile_index != layer_mid.try_get(tile_pos + ivec2(2,1).rot90(i)) ||
                                tile_index != layer_mid.try_get(tile_pos + ivec2(2,2).rot90(i)) ||
                                tile_index != layer_mid.try_get(tile_pos + ivec2(1,2).rot90(i)))
                            {
                                same_adjacent = 0;
                                break;
                            }
                        }

                        if (same_adjacent)
                            depth++;
                    }

                    for (ivec2 piece_pos : vector_range(ivec2(2)))
                    {
                        ivec2 offset = piece_pos * 2 - 1;

                        int piece_variant;

                        if (depth == 2)
                        {
                            piece_variant = 6;
                        }
                        else if (depth == 1)
                        {
                            piece_variant = 5;
                        }
                        else
                        {
                            bool same_x  = tile_index == layer_mid.try_get(tile_pos.add_x(offset.x));
                            bool same_y  = tile_index == layer_mid.try_get(tile_pos.add_y(offset.y));
                            bool same_xy = tile_index == layer_mid.try_get(tile_pos + offset);

                            if (same_x && same_y && same_xy)
                                piece_variant = 0;
                            else if (same_x && same_y)
                                piece_variant = 4;
                            else if (same_x)
                                piece_variant = 2;
                            else if (same_y)
                                piece_variant = 1;
                            else
                                piece_variant = 3;
                        }

                        r.iquad(pixel_pos + piece_pos * tile_size/2, images.tiles.Region(info.tex_pos.add_y(piece_variant) * tile_size + piece_pos * tile_size/2, ivec2(tile_size/2)));
                    }
                }
                break;
              default:
                Program::Error("Unknown tile render mode: ", int(info.render_mode), ".");
            }
        }
    }
};

struct Hitbox
{
    ivec2 half_extent = ivec2(0);
    std::vector<ivec2> points;

    Hitbox() {}
    Hitbox(ivec2 half_extent) : half_extent(half_extent)
    {
        ivec2 size = half_extent * 2;
        for (ivec2 pos : vector_range((size - 2) / Map::tile_size + 2))
            points.push_back(clamp_max(pos * Map::tile_size, size-1) - half_extent);
    }

    template <typename F> void ForEachCollidingTile(const Tiled::TileLayer &layer, ivec2 pos, F &&func) const // `func` is `bool func(ivec2 tile_pos, int tile_index)`. If it returns `false`, the loop stops.
    {
        for (ivec2 point : points)
        {
            ivec2 tile_pos = div_ex(pos + point, Map::tile_size);
            if (!layer.pos_in_range(tile_pos))
                continue;

            int tile_index = layer.try_get(tile_pos);
            bool should_continue = func(std::as_const(tile_pos), std::as_const(tile_index));

            if (!should_continue)
                break;
        }
    }

    bool IsSolidAt(const Tiled::TileLayer &layer, ivec2 pos) const
    {
        bool ret = 0;
        ForEachCollidingTile(layer, pos, [&](ivec2 /*tile_pos*/, int tile_index) -> bool
        {
            if (tile_index == 0)
                return 1;

            Tiles::Category category = Tiles::GetInfo(tile_index).category;

            if (category != Tiles::solid)
                return 1;

            ret = 1;
            return 0;
        });
        return ret;
    }

    bool IsDangerousAt(const Tiled::TileLayer &layer, ivec2 pos) const
    {
        bool ret = 0;
        ForEachCollidingTile(layer, pos, [&](ivec2 /*tile_pos*/, int tile_index) -> bool
        {
            if (tile_index == 0)
                return 1;

            Tiles::Category category = Tiles::GetInfo(tile_index).category;

            if (category != Tiles::kills)
                return 1;

            ret = 1;
            return 0;
        });
        return ret;
    }

};

struct Camera
{
    fvec2 pos = fvec2(0);
    fvec2 vel = fvec2(0);
    ivec2 pos_int = ivec2(0);

    std::vector<fvec2> targets;

    void SetPos(fvec2 new_pos)
    {
        pos = new_pos;
        vel = fvec2(0);
        pos_int = iround(pos);
    }

    void AddTarget(fvec2 point)
    {
        targets.push_back(point);
    }

    void Tick()
    {
        if (targets.size() > 0)
        {
            fvec2 target_point = std::accumulate(targets.begin(), targets.end(), fvec2(0)) / targets.size();
            targets.clear();

            fvec2 delta = target_point - pos;
            float dist = delta.len();

            if (dist < 0.0001)
                return;

            fvec2 dir = delta / dist;

            vel += dir * pow(dist / config.camera.ref_distance, config.camera.power) / config.camera.mass;
        }

        vel *= (1 - config.camera.drag);
        pos += vel;

        pos_int = iround(pos);
    }

    void RenderBackground() const
    {
        ivec2 base_pos = mod_ex(iround(pos / -2.), images.bg1.size) - screen_size / 2 - images.bg1.size;
        ivec2 tile_count((screen_size + images.bg1.size - 1) / images.bg1.size + 1);

        for (ivec2 tile_pos : vector_range(tile_count))
            r.iquad(tile_pos * images.bg1.size + base_pos, images.bg1);
    }
};

class DamageController
{
  public:
    enum Team {team_player, team_enemies};

  private:
    struct Melee
    {
        Team team = team_enemies;
        fvec2 pos = fvec2(0);
        fvec2 half_size = fvec2(0);
        fvec2 knockback = fvec2(0,-1);
    };

    std::vector<Melee> melee_objects;
    std::vector<Melee> melee_objects_next;

  public:
    DamageController() {}

    void AddMelee(Team team, fvec2 pos, fvec2 size, fvec2 knockback)
    {
        Melee &obj = melee_objects_next.emplace_back();
        obj.team = team;
        obj.pos = pos;
        obj.half_size = size/2;
        obj.knockback = knockback;
    }

    void Tick()
    {
        melee_objects = std::move(melee_objects_next);
        melee_objects_next.clear();
    }

    void Render(ivec2 cam_pos) const
    {
        auto draw_melee = [&](const Melee &melee)
        {
            r.iquad(iround(melee.pos - cam_pos), iround(melee.half_size * 2)).center().color(fvec3(1,0,0)).alpha(0.5);
        };

        for (const auto &melee : melee_objects)
            draw_melee(melee);
        for (const auto &melee : melee_objects_next)
            draw_melee(melee);

    }

    bool PosDangerous(Team team, fvec2 pos, fvec2 size, fvec2 *dir = 0)
    {
        fvec2 half_size = size / 2;

        if (dir)
            *dir = fvec2(0);

        for (const Melee &melee_obj : melee_objects)
        {
            if (melee_obj.team == team)
                continue;

            if ((abs(melee_obj.pos - pos) < melee_obj.half_size + half_size).all())
            {
                if (dir)
                    *dir = melee_obj.knockback;
                return 1;
            }
        }

        return 0;
    }
};

class ParticleControler
{
    struct Particle
    {
        fvec2 pos = fvec2(0);
        fvec2 vel = fvec2(0);
        float drag = 0;
        float size = 2;
        fvec2 visual_dir;
        int ticks = 0;
        int mid_ticks = 0;
        int max_ticks = 0;
        fvec3 color = fvec3(1);
        float alpha = 1;
        float beta = 1;
    };

    std::deque<Particle> particles;

  public:
    ParticleControler() {}

    void Tick()
    {
        for (Particle &par : particles)
        {
            par.pos += par.vel;
            par.vel *= (1 - par.drag);
            par.ticks++;
        }

        particles.erase(std::remove_if(particles.begin(), particles.end(), [](const Particle &par){return par.ticks >= par.max_ticks;}), particles.end());
    }

    void Render(ivec2 cam_pos, ivec2 viewport_size) const
    {
        for (const Particle &par : particles)
        {
            if ((abs(par.pos - cam_pos) > viewport_size/2 + par.size).any())
                continue;

            float t;
            if (par.ticks < par.mid_ticks)
                t = par.ticks / float(par.mid_ticks);
            else
                t = 1 - (par.ticks - par.mid_ticks) / float(par.max_ticks - par.mid_ticks);
            clamp_var(t);

            r.fquad(par.pos - cam_pos, fvec2(par.size * t)).center().matrix(fmat2(par.visual_dir, par.visual_dir.rot90())).color(par.color).alpha(par.alpha).beta(par.beta);
        }
    }

    void AddParticle(fvec2 pos, fvec2 vel, float drag, float size, float angle, int ticks, int mid_ticks, int max_ticks, fvec3 color, float alpha = 1, float beta = 1)
    {
        Particle &par = particles.emplace_back();
        par.pos = pos;
        par.vel = vel;
        par.drag = drag;
        par.size = size;
        par.visual_dir = fvec2::dir(angle);
        par.ticks = ticks;
        par.mid_ticks = mid_ticks;
        par.max_ticks = max_ticks;
        par.color = color;
        par.alpha = alpha;
        par.beta = beta;
    }

    void EffectDeath(fvec2 pos, fvec2 vel, float rad, int particle_count, int max_size, float max_vel)
    {
        for (int i = 0; i < particle_count; i++)
        {
            fvec2 p = pos + fvec2::dir(random.angle(), sqrt(float(0 <= random.real() <= 1)) * rad);
            fvec2 v = vel + fvec2::dir(random.angle(), max_vel * 0.1 <= random.real() <= max_vel);
            float size = max_size * 0.1 <= random.real() <= max_size;
            float color_t = 0 <= random.real() <= 1;
            fvec3 color = (fvec3(227,35,11) * (1 - color_t) + fvec3(133,4,43) * color_t) / 255;

            float time = 0.1 <= random.real() <= 1;

            AddParticle(p, v, 0.02, size, random.angle(), 0, 15 * time, 90 * time, color, 0.8 <= random.real() <= 1);
        }
    }
};

class SpeechController
{
    bool active = 0;
    int ticks_before_next_letter = 0;
    std::vector<std::string> current_text;
    std::string target_text;
    float position = 0;
    bool need_key_press = 0;

    Input::Button *key = &Keys::attack;

  public:
    SpeechController() {}

    bool IsBlocking() const
    {
        return active || position > 0.001;
    }

    void Say(std::string new_text)
    {
        active = 1;
        target_text = new_text;
        current_text = {};
        ticks_before_next_letter = config.speech.ticks_per_letter;
        need_key_press = 0;
    }

    void Tick()
    {
        clamp_var(position += (active ? 1 : -1) / float(config.speech.movement_anim_ticks));

        if (active)
        {
            if (position >= 0.999)
            {
                need_key_press = target_text.empty() || target_text.front() == '\n';

                if (!need_key_press || key->pressed())
                {
                    if (need_key_press && key->pressed())
                    {
                        Sounds::dialogue_click(screen_size/2);
                        ticks_before_next_letter = 0;
                    }

                    if (target_text.empty())
                    {
                        active = 0;
                    }
                    else
                    {
                        if (target_text.front() == '\n' || current_text.empty())
                        {
                            current_text.emplace_back();
                            while (int(current_text.size()) > config.speech.max_lines)
                                current_text.erase(current_text.begin());
                        }
                        auto &last_line = current_text.back();

                        ticks_before_next_letter--;
                        if (ticks_before_next_letter <= 0)
                        {
                            ticks_before_next_letter = config.speech.ticks_per_letter;
                            last_line += target_text.front();
                            target_text.erase(target_text.begin());

                            if (last_line.back() != ' ' && last_line.back() != '\n')
                            {
                                if (!(need_key_press && key->pressed()))
                                {
                                    auto symbol_pos = Strings::GetSymbolPosition(last_line.c_str(), &last_line.back());
                                    ivec2 sound_pos = ivec2(symbol_pos.column, symbol_pos.line) * ivec2(6,font.LineSkip()) + ivec2(-screen_size.x/2, screen_size.y/2 - images.dialogue.size.y) + config.speech.text_offset;
                                    Sounds::death_speaks(sound_pos);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    void Render() const
    {
        if (position <= 0.001)
            return;

        float t = smoothstep(position);
        int offset = iround(-images.dialogue.size.y * t);

        // Frame
        r.iquad(ivec2(-screen_size.x/2, screen_size.y/2 + offset), images.dialogue);

        // Text
        for (size_t i = 0; i < current_text.size(); i++)
            r.itext(ivec2(-screen_size.x/2, screen_size.y/2 + offset + i * font.LineSkip()) + config.speech.text_offset, Graphics::Text(font, current_text[i])).color(fvec3(0.8)).align(ivec2(-1));

        // Key hint
        if (need_key_press && int(window.Ticks() % config.speech.key_hint_blink_period) < config.speech.key_hint_blink_period / 2)
        {
            std::string key_hint_text = "Press {}"_format(key->Name());
            r.itext(ivec2(screen_size.x/2, screen_size.y/2 + offset + images.dialogue.size.y) + config.speech.key_hint_offset, Graphics::Text(font, key_hint_text)).color(fvec3(0.5)).align(ivec2(1));
        }
    }
};

struct Env
{
    Map map;
    DamageController dmg;
    ParticleControler par;
    SpeechController speech;
    Camera cam;
};

namespace States // Current state definition
{
    struct Base : Meta::polymorphic<Base>
    {
        virtual void Tick() = 0;
        virtual void Render() const = 0;
        virtual ~Base() = default;
    };

    Poly::Storage<Base> current_state;
}

namespace Entities
{
    struct HasHitbox
    {
        const Hitbox *hitbox = 0;

        virtual bool IsSolidAtAbsolutePos(const Tiled::TileLayer &layer, ivec2 pos) const
        {
            if (!hitbox)
                Program::Error("Entity has null hitbox.");

            return hitbox->IsSolidAt(layer, pos) || hitbox->IsDangerousAt(layer, pos);
        }

        ivec2 HitboxSize() const
        {
            if (!hitbox)
                Program::Error("Entity has null hitbox.");

            return hitbox->half_extent * 2;
        }
    };

    struct Movable
    {
        ivec2 pos = ivec2(0);
        fvec2 vel = ivec2(0);
        fvec2 vel_lag = ivec2(0);

        void UpdatePosition()
        {
            fvec2 real_vel = vel + vel_lag;
            ivec2 int_vel = iround(real_vel);
            vel_lag = real_vel - int_vel;
            vel_lag *= 0.99;

            pos += int_vel;
        }
    };

    struct Physical : Movable, HasHitbox
    {
        bool ground = 0;

        bool IsSolidAtOffset(const Tiled::TileLayer &layer, ivec2 offset) const
        {
            return IsSolidAtAbsolutePos(layer, pos + offset);
        }

        void UpdatePositionAvoidingWalls(Env &env)
        {
            ivec2 new_pos = pos;
            UpdatePosition();
            std::swap(pos, new_pos);

            while (pos != new_pos)
            {
                ivec2 delta_sign = sign(new_pos - pos);

                if (delta_sign == ivec2(0))
                    break;

                if (delta_sign.x)
                {
                    if (IsSolidAtOffset(env.map.layer_mid, delta_sign.set_y(0)))
                    {
                        new_pos.x = pos.x;
                        if (vel.x * delta_sign.x > 0)
                            vel.x = 0;
                        if (vel_lag.x * delta_sign.x > 0)
                            vel_lag.x = 0;
                    }
                    else
                    {
                        pos.x += delta_sign.x;
                    }
                }

                if (delta_sign.y)
                {
                    if (IsSolidAtOffset(env.map.layer_mid, delta_sign.set_x(0)))
                    {
                        new_pos.y = pos.y;
                        if (vel.y * delta_sign.y > 0)
                            vel.y = 0;
                        if (vel_lag.y * delta_sign.y > 0)
                            vel_lag.y = 0;
                    }
                    else
                    {
                        pos.y += delta_sign.y;
                    }
                }
            }

            ground = IsSolidAtOffset(env.map.layer_mid, ivec2(0,1));
        }
    };

    struct PhysicalKillable : Physical
    {
        bool IsSolidAtAbsolutePos(const Tiled::TileLayer &layer, ivec2 pos) const override
        {
            if (!hitbox)
                Program::Error("Entity has null hitbox.");

            if (IsDead())
                return 0;

            return hitbox->IsSolidAt(layer, pos);
        }

        int dead_ticks = 0;

        bool IsDead() const
        {
            return dead_ticks != 0;
        }

        virtual void BecomesDead(Env &env) {(void)env;}

        void CheckDamage(Env &env, DamageController::Team team)
        {
            if (!hitbox)
                Program::Error("Entity has null hitbox.");

            if (dead_ticks)
            {
                dead_ticks++;
                return;
            }

            fvec2 knockback;
            if (env.dmg.PosDangerous(team, pos, hitbox->half_extent * 2, &knockback))
            {
                dead_ticks = 1;
                vel += knockback;
                BecomesDead(env);
                return;
            }

            if (hitbox->IsDangerousAt(env.map.layer_mid, pos))
            {
                dead_ticks = 1;
                BecomesDead(env);
                return;
            }
        }

        void UpdatePositionAvoidingWalls(Env &env, DamageController::Team team)
        {
            Physical::UpdatePositionAvoidingWalls(env);
            CheckDamage(env, team);
        }
    };

    struct Controllable : Meta::polymorphic<Controllable>, PhysicalKillable
    {
        int h_dir = 1; // This shouldn't be equal to 0.

        virtual void Tick(Env &env)
        {
            (void)env;
        }
        virtual void Render(const Env &env) const
        {
            (void)env;
        }
    };

    struct Controller : Meta::polymorphic<Controller>
    {
        virtual bool HoldLeft() const {return 0;}
        virtual bool HoldRight() const {return 0;}
        virtual bool PressJump() const {return 0;}
        virtual bool HoldJump() const {return 0;}
        virtual bool PressAttack() const {return 0;}

        virtual void PreTick(Env &env, Controllable &) {(void)env;}
        virtual void PostTick(Env &env, Controllable &) {(void)env;}
        virtual void PreRender(const Env &env, const Controllable &) const {(void)env;}
        virtual void PostRender(const Env &env, const Controllable &) const {(void)env;}

        virtual DamageController::Team GetTeam() const {return DamageController::team_enemies;}
    };

    struct Knight : Controllable
    {
        static constexpr int hat_count = 6;

        Poly::Storage<Controller> controller;

        int jump_ticks_left = 0;
        int attack_timer = 0;
        float attack_anim_state = -1;
        int walk_ticks = 0;
        int hat_index = 0;

        Knight()
        {
            static Hitbox entity_hitbox(ivec2(10,16) / 2);
            hitbox = &entity_hitbox;

            hat_index = (0 <= random.integer() <= hat_count);
        }

        void Tick(Env &env) override
        {
            controller->PreTick(env, *this);

            // Gravity
            if (!IsDead())
                vel.y += config.global.gravity;

            { // Walking
                int control = controller->HoldRight() - controller->HoldLeft();

                if (control && (sign(control) != -sign(vel.x) || abs(vel.x) < 0.05))
                {
                    vel.x += control * config.humans.walk_acc;

                    if (abs(vel.x) > config.humans.walk_speed_x)
                        vel.x = sign(vel.x) * config.humans.walk_speed_x;

                    h_dir = control;
                }
                else
                {
                    if (abs(vel.x) <= config.humans.walk_dec)
                        vel.x = 0;
                    else
                        vel.x -= sign(vel.x) * config.humans.walk_dec;
                }
            }

            { // Walking animation
                if (abs(vel.x) > 0.01)
                {
                    walk_ticks++;
                    walk_ticks %= config.humans.knights.walk_anim_ticks;
                }
                else
                {
                    walk_ticks = 0;
                }
            }

            { // Jumping
                if (ground && controller->PressJump())
                {
                    jump_ticks_left = config.humans.jump_ticks;
                }
                else if (!controller->HoldJump())
                {
                    jump_ticks_left = 0;
                }

                if (jump_ticks_left)
                {
                    jump_ticks_left--;
                    vel.y = -config.humans.jump_speed;
                }
            }

            { // Attacking
                if (controller->PressAttack() && attack_timer == 0)
                {
                    attack_timer = config.humans.knights.attack_delay + config.humans.knights.attack_cooldown;
                }

                if (attack_timer > 0)
                {
                    if (attack_timer == config.humans.knights.attack_cooldown) // Sic.
                    {
                        attack_anim_state = 0;
                        ivec2 attack_pos = pos + config.humans.knights.attack_center_offset.mul_x(h_dir);
                        env.dmg.AddMelee(controller->GetTeam(), attack_pos, config.humans.knights.attack_size, ivec2(h_dir, 0));
                        Sounds::sword_attack(attack_pos);
                    }

                    attack_timer--;
                }

                if (attack_anim_state > -0.5)
                {
                    attack_anim_state += 1 / config.humans.knights.attack_anim_ticks;

                    if (attack_anim_state >= 1)
                        attack_anim_state = -1;
                }
            }

            UpdatePositionAvoidingWalls(env, controller->GetTeam());
            controller->PostTick(env, *this);
        }

        void Render(const Env &env) const override
        {
            controller->PreRender(env, *this);

            ivec2 screen_pos = pos - env.cam.pos_int;

            { // Body
                // Lower body
                constexpr int walk_anim_frames = 4;
                int legs_state;
                if (ground == 0)
                    legs_state = 5;
                else if (walk_ticks == 0)
                    legs_state = 0;
                else
                    legs_state = 1 + clamp_max(iround(floor(walk_ticks / float(config.humans.knights.walk_anim_ticks) * walk_anim_frames)), walk_anim_frames-1);
                r.iquad(screen_pos, images.humans.Region(ivec2(legs_state * 32,32), ivec2(32))).center().alpha(clamp(1 - dead_ticks / 15.)).flip_x(h_dir < 0);

                // Upper body and hat
                ivec2 body_offset = ivec2(0);
                if (legs_state > walk_anim_frames/2) // Sic.
                    body_offset = ivec2(0,-1);
                if (legs_state == 0)
                    body_offset = ivec2(0, iround(0.5 + 0.5 * sin(window.Ticks() % config.humans.knights.stand_anim_ticks / float(config.humans.knights.stand_anim_ticks) * f_pi * 2)));
                int body_state = 0;
                if (attack_timer > config.humans.knights.attack_cooldown) // Sic.
                    body_state = 1;
                // Hat
                r.iquad(screen_pos + body_offset, images.humans.Region(ivec2(hat_index * 32,32*2), ivec2(32))).center().alpha(clamp(1 - dead_ticks / 15.)).flip_x(h_dir < 0);
                // Upper body
                r.iquad(screen_pos + body_offset, images.humans.Region(ivec2(body_state * 32,0), ivec2(32))).center().alpha(clamp(1 - dead_ticks / 15.)).flip_x(h_dir < 0);
            }

            // Attack
            if (attack_anim_state > -0.5)
            {
                constexpr int attack_frames = 4;
                constexpr int effect_size = 32;

                int frame = clamp(iround(floor(attack_anim_state * attack_frames)), 0, attack_frames-1);
                r.iquad(screen_pos, images.effects.Region(ivec2(frame,0) * effect_size, ivec2(effect_size))).center().flip_x(h_dir < 0);
            }

            controller->PostRender(env, *this);
        }

        void BecomesDead(Env &env) override
        {
            controller = Poly::make;
            env.par.EffectDeath(pos, vel / 2, 5, 75, 10, 1.5);
            Sounds::death(pos);
        }
    };

    struct Controller_Player : Controller
    {
        DamageController::Team GetTeam() const override
        {
            return DamageController::team_player;
        }

        bool HoldLeft() const override
        {
            return Keys::left.down();
        }
        bool HoldRight() const override
        {
            return Keys::right.down();
        }
        bool PressJump() const override
        {
            return Keys::jump.pressed();
        }
        bool HoldJump() const override
        {
            return Keys::jump.down();
        }
        bool PressAttack() const override
        {
            return Keys::attack.pressed();
        }
    };
}

namespace States
{
    struct Game : Base
    {
        static Game saved_game;

        Poly::Storage<Entities::Controllable> p;
        Env env;
        std::vector<Poly::Storage<Entities::Controllable>> enemies;

        float death_fade = 0;

        Game() {}

        Game(std::string map_name)
        {
            // Load map.
            env.map = Map(map_name);

            { // Set up player.
                auto &knight = p.assign<Entities::Knight>();
                knight.pos = env.map.points.GetSinglePoint("player");
                knight.controller = Poly::make_derived<Entities::Controller_Player>;
                env.cam.SetPos(knight.pos + ivec2(0,config.camera.y_offset_to_player));
            }

            { // Set up enemies
                for (ivec2 pos : env.map.points.GetPointList("knight"))
                {
                    auto &knight = enemies.emplace_back().assign<Entities::Knight>();
                    knight.pos = pos;
                    knight.controller = Poly::make;
                }
            }

            // Save game.
            saved_game = *this;
        }

        void Tick() override
        {
            { // Audio
                Audio::Listener::Position(env.cam.pos.to_vec3(-config.audio.distance * screen_size.x/2));
                Audio::Listener::Orientation(fvec3(0,0,1), fvec3(0,-1,0));
                Audio::Source::DefaultRefDistance(config.audio.ref_distance * screen_size.x/2);
                Audio::Source::DefaultMaxDistance(config.audio.max_distance * screen_size.x/2);
                Audio::Source::DefaultRolloffFactor(config.audio.rolloff_factor);
            }

            // Dialogues
            env.speech.Tick();

            if (!env.speech.IsBlocking())
            {
                // Player
                p->Tick(env);

                // Enemies
                for (auto &enemy : enemies)
                    enemy->Tick(env);

                // Particles
                env.par.Tick();

                // Camera
                if (!p->IsDead())
                    env.cam.AddTarget(p->pos + ivec2(0,config.camera.y_offset_to_player));
                env.cam.Tick();

                // Damage controler
                env.dmg.Tick();
            }

            // Death fade
            clamp_var(death_fade += (p->dead_ticks > config.global.death_fade_delay ? 1 : -1) / float(config.global.death_fade_ticks));
            if (death_fade > 0.9999)
            {
                *this = saved_game;
                death_fade = 1;
            }

            // Debug gui
            ConfigData::DisplayGui();

            if (Input::Button(Input::space).pressed())
                env.speech.Say("You fool!...\nDo you understand what you did?");
        }

        void Render() const override
        {
            // Pre
            Graphics::SetClearColor(fvec3(0));
            Graphics::Clear();
            r.BindShader();

            // Background
            //r.iquad(ivec2(0), screen_size).center().color(fvec3(0.5));
            env.cam.RenderBackground();

            // Map
            env.map.Render(env.cam.pos_int);

            // Enemies
            for (const auto &enemy : enemies)
                enemy->Render(env);

            // Player
            p->Render(env);

            // Particles
            env.par.Render(env.cam.pos_int, screen_size);

            // Darkness
            r.iquad(ivec2(0), images.darkness).alpha(config.global.darkness_alpha).center();

            // Dialogues
            env.speech.Render();

            // Damage sources (debug)
            if (config.debug.draw_damage_areas)
                env.dmg.Render(env.cam.pos_int);

            { // Death fade
                if (death_fade > 0.0001)
                {
                    float t = smoothstep(death_fade);

                    for (int i = 0; i < 2; i++)
                        r.iquad(ivec2(0,(1-t) * screen_size.y/2 * (i ? -1 : 1)), ivec2(screen_size.x,screen_size.y/2)).center(ivec2(screen_size.x/2, 0)).rotate(f_pi * i).color(fvec3(0));

                    r.iquad(ivec2(0), screen_size).center().color(fvec3(0)).alpha(t);
                }
            }

            // Post
            r.Finish();
        }
    };

    Game Game::saved_game;
}

int main()
{
    { // Initialize
        ImGui::StyleColorsDark();
        ImGui::GetStyle().FrameRounding = 0;
        ImGui::GetStyle().ChildRounding = 0;
        ImGui::GetStyle().WindowRounding = 0;

        Graphics::Blending::Enable();
        Graphics::Blending::FuncNormalPre();
    }

    auto Resize = [&]
    {
        adaptive_viewport.Update();
        mouse.SetMatrix(adaptive_viewport.GetDetails().MouseMatrixCentered());
    };
    Resize();

    auto ReloadTextures = [&]
    {
        texture_atlas = TextureAtlas(ivec2(1024), "assets/_images", "assets/atlas.png", "assets/atlas.refl");

        std::string symbols     = "abcdefghijklmnopqrstuvwxyz .,!?\"'-";
        std::string symbols_alt = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

        font.SetAscent(6);
        font.SetDescent(1);
        font.SetLineSkip(8);
        auto font_image = texture_atlas.Get("font.png");
        for (size_t i = 0; i < symbols.size(); i++)
        {
            auto lambda = [&](Graphics::Font::Glyph &glyph)
            {
                glyph.advance = 6;
                glyph.offset = ivec2(0,-6);
                glyph.size = ivec2(6,8);
                glyph.texture_pos = font_image.pos + ivec2(6 * i, 0);
            };

            lambda(font.Insert(symbols[i]));
            if (i < symbols_alt.size())
                lambda(font.Insert(symbols_alt[i]));
        }
        font.DefaultGlyph() = font.Insert('?');

        images = Images(texture_atlas);
        texture_main.SetData(texture_atlas.GetImage());
    };
    ReloadTextures();
    r.SetTexture(texture_main);

    States::current_state = {Poly::make_derived<States::Game>, "prologue"};

    Metronome metronome(60);
    Clock::DeltaTimer delta_timer;

    while (1)
    {
        uint64_t delta = delta_timer();
        while (metronome.Tick(delta))
        {
            window.ProcessEvents({gui_controller.EventHook()});

            if (window.Resized())
            {
                Resize();
                Graphics::Viewport(window.Size());
            }
            if (window.ExitRequested())
                Program::Exit();

            gui_controller.PreTick();
            States::current_state->Tick();
            audio_context.Tick();
            Audio::CheckErrors();

            if (Input::Button(Input::f5).pressed())
                ReloadTextures();
        }

        gui_controller.PreRender();
        adaptive_viewport.BeginFrame();
        States::current_state->Render();
        adaptive_viewport.FinishFrame();
        Graphics::CheckErrors();
        gui_controller.PostRender();

        window.SwapBuffers();
    }
}
