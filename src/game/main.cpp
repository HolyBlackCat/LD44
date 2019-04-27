#include "config_data.h"
#include "image_data.h"

//{ Resources
constexpr ivec2 screen_size(480, 270);
Interface::Window window("Delta", screen_size * 2, Interface::windowed, ADJUST_G(Interface::WindowSettings{}, min_size = screen_size));
Graphics::DummyVertexArray dummy_vao;

const Graphics::ShaderConfig shader_config = Graphics::ShaderConfig::Core();
Interface::ImGuiController gui_controller(shader_config.common_header);

TextureAtlas texture_atlas(ivec2(1024), "assets/_images", "assets/atlas.png", "assets/atlas.refl");
Graphics::Texture texture_main = Graphics::Texture().Wrap(Graphics::clamp).Interpolation(Graphics::nearest).SetData(texture_atlas.GetImage());
Render r = ADJUST_G(Render(0x2000, shader_config), SetTexture(texture_main), SetMatrix(fmat4::ortho(screen_size/ivec2(-2,2), screen_size/ivec2(2,-2), -1, 1)));
AdaptiveViewport adaptive_viewport(shader_config, screen_size);

Input::Mouse mouse;

Images images(texture_atlas);
//}


namespace Tiles
{
    enum RenderMode
    {
        simple,
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
        /* 0,0 - wall  */ {simple, ivec2(0,0), solid},
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

namespace Entities
{
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
            fvec2 target_point = std::accumulate(targets.begin(), targets.end(), fvec2(0)) / targets.size();
            targets.clear();

            fvec2 delta = target_point - pos;
            float dist = delta.len();
            fvec2 dir = delta / dist;

            vel += dir * pow(dist / config.camera.ref_distance, config.camera.power) / config.camera.mass;
            vel *= (1 - config.camera.drag);
            pos += vel;

            pos_int = iround(pos);
        }
    };


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

        virtual void UpdatePositionAvoidingWalls(const Tiled::TileLayer &layer)
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
                    if (IsSolidAtOffset(layer, delta_sign.set_y(0)))
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
                    if (IsSolidAtOffset(layer, delta_sign.set_x(0)))
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

            ground = IsSolidAtOffset(layer, ivec2(0,1));
        }
    };

    struct PhysicalKillable : Physical
    {
        bool IsSolidAtAbsolutePos(const Tiled::TileLayer &layer, ivec2 pos) const override
        {
            if (!hitbox)
                Program::Error("Entity has null hitbox.");

            return hitbox->IsSolidAt(layer, pos);
        }

        int dead_ticks = 0;

        bool IsDead() const
        {
            return dead_ticks != 0;
        }

        virtual void BecomesDead() {}

        void CheckDamage(const Tiled::TileLayer &layer)
        {
            if (!hitbox)
                Program::Error("Entity has null hitbox.");

            if (dead_ticks)
            {
                dead_ticks++;
                return;
            }

            if (hitbox->IsDangerousAt(layer, pos))
            {
                dead_ticks = 1;
                BecomesDead();
            }
        }

        void UpdatePositionAvoidingWalls(const Tiled::TileLayer &layer) override
        {
            Physical::UpdatePositionAvoidingWalls(layer);
            CheckDamage(layer);
        }
    };

    struct Human : PhysicalKillable
    {
        struct Controller : Meta::polymorphic<Controller>
        {
            virtual bool HoldLeft() const {return 0;}
            virtual bool HoldRight() const {return 0;}
            virtual bool PressJump() const {return 0;}
            virtual bool HoldJump() const {return 0;}

            virtual void PreTick(Human &) {}
            virtual void PostTick(Human &) {}
            virtual void PreRender(const Human &) const {}
            virtual void PostRender(const Human &) const {}
        };
        Poly::Storage<Controller> controller;

        int jump_ticks_left = 0;

        Human()
        {
            static Hitbox entity_hitbox(ivec2(10,16) / 2);

            hitbox = &entity_hitbox;
        }

        void Tick(const Tiled::TileLayer &layer)
        {
            controller->PreTick(*this);

            // Gravity
            vel.y += config.global.gravity;

            { // Walking
                int control = controller->HoldRight() - controller->HoldLeft();
                if (control && (sign(control) != -sign(vel.x) || abs(vel.x) < 0.05))
                {
                    vel.x += control * config.humans.walk_acc;

                    if (abs(vel.x) > config.humans.walk_speed_x)
                        vel.x = sign(vel.x) * config.humans.walk_speed_x;
                }
                else
                {
                    if (abs(vel.x) <= config.humans.walk_dec)
                        vel.x = 0;
                    else
                        vel.x -= sign(vel.x) * config.humans.walk_dec;
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

            UpdatePositionAvoidingWalls(layer);
            controller->PostTick(*this);
        }

        void Render(ivec2 cam_pos) const
        {
            controller->PreRender(*this);

            ivec2 screen_pos = pos - cam_pos;
            r.iquad(screen_pos, images.human.Region(ivec2(0), ivec2(32))).center().alpha(clamp(1 - dead_ticks / 30.));

            controller->PostRender(*this);
        }

        void BecomesDead() override
        {
            controller = Poly::make;
        }
    };

    struct HumanController_Player : Human::Controller
    {
        Reflect(HumanController_Player)
        (
            (Input::Button)(left) (=Input::left ),
            (Input::Button)(right)(=Input::right),
            (Input::Button)(jump) (=Input::z    ),
        )

        bool HoldLeft() const
        {
            return left.down();
        }
        bool HoldRight() const
        {
            return right.down();
        }
        bool PressJump() const
        {
            return jump.pressed();
        }
        bool HoldJump() const
        {
            return jump.down();
        }
    };
}

namespace States
{
    struct Base : Meta::polymorphic<Base>
    {
        virtual void Tick() = 0;
        virtual void Render() const = 0;
        virtual ~Base() = default;
    };

    Poly::Storage<Base> current_state;

    struct Game : Base
    {
        static Game saved_game;

        Map map;
        Entities::Human p;
        Entities::Camera cam;

        float death_fade = 0;

        Game() {}

        Game(std::string map_name)
        {
            map = Map(map_name);
            p.controller = Poly::make_derived<Entities::HumanController_Player>;
            p.pos = map.points.GetSinglePoint("player");

            saved_game = *this;
        }

        void Tick() override
        {
            p.Tick(map.layer_mid);
            cam.AddTarget(p.pos + ivec2(0,config.camera.y_offset_to_player));
            cam.Tick();


            clamp_var(death_fade += (p.IsDead() ? 1 : -1) / float(config.global.death_fade_ticks));
            if (death_fade > 0.9999)
            {
                *this = saved_game;
                death_fade = 1;
            }

            ConfigData::DisplayGui();
        }

        void Render() const override
        {
            // Pre
            Graphics::SetClearColor(fvec3(0));
            Graphics::Clear();
            r.BindShader();

            // Background
            r.iquad(ivec2(0), screen_size).center().color(fvec3(1));

            // Map
            map.Render(cam.pos_int);

            // Player
            p.Render(cam.pos_int);

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

        Graphics::Blending::Enable();
        Graphics::Blending::FuncNormalPre();
    }

    auto Resize = [&]
    {
        adaptive_viewport.Update();
        mouse.SetMatrix(adaptive_viewport.GetDetails().MouseMatrixCentered());
    };
    Resize();

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
