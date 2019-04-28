#pragma once

#include <string>
#include "utils/mat.h"

struct ConfigData
{
    ReflectStruct(Debug,(
        (bool)(draw_damage_areas)(=1),
    ))

    ReflectStruct(Audio,(
        (float)(volume)(=1),
        (float)(distance)(=1), // Distance is multiplied by (screen width / 2).
        (float)(ref_distance)(=4),
        (float)(max_distance)(=4),
        (float)(rolloff_factor)(=1),
    ))

    ReflectStruct(Global,(
        (float)(darkness_alpha)(=1),
        (float)(gravity)(=0),
        (int)(death_fade_ticks)(=10),
        (int)(death_fade_delay)(=30),
    ))

    ReflectStruct(Camera,(
        (float)(ref_distance)(=100),
        (float)(power)(=1.5),
        (float)(mass)(=50),
        (float)(drag)(=0.01),
        (float)(y_offset_to_player)(=-32),
    ))

    ReflectStruct(Speech, (
        (int)(max_lines)(=3),
        (int)(ticks_per_letter)(=5),
        (ivec2)(text_offset)(=ivec2(32,16)),
        (ivec2)(key_hint_offset)(=ivec2(-16)),
        (int)(key_hint_blink_period)(=10),
        (int)(movement_anim_ticks)(=30),
    ))

    ReflectStruct(Knights,(
        (int)(attack_delay,attack_cooldown)(=8),
        (ivec2)(attack_center_offset)(=ivec2(10,0)),
        (ivec2)(attack_size)(=ivec2(8)),
        (float)(attack_anim_ticks)(=8),
        (int)(walk_anim_ticks)(=30),
        (int)(stand_anim_ticks)(=170),
    ))

    ReflectStruct(Humans,(
        (Knights)(knights),
        (float)(walk_acc,walk_dec)(=0.01),
        (float)(walk_speed_x)(=1),
        (float)(jump_speed)(=1),
        (int)(jump_ticks)(=4),
    ))

    Reflect(ConfigData)
    (
        (Debug)(debug),
        (Audio)(audio),
        (Global)(global),
        (Camera)(camera),
        (Speech)(speech),
        (Humans)(humans),
    )

    static void DisplayGui();
};

extern const ConfigData &config;
