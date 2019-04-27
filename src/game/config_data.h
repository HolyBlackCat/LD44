#pragma once

#include <string>
#include "utils/mat.h"

struct ConfigData
{
    ReflectStruct(Global,(
        (float)(gravity)(=0),
        (int)(death_fade_ticks)(=10),
    ))

    ReflectStruct(Camera,(
        (float)(ref_distance)(=100),
        (float)(power)(=1.5),
        (float)(mass)(=50),
        (float)(drag)(=0.01),
        (float)(y_offset_to_player)(=-32),
    ))

    ReflectStruct(Humans,(
        (float)(walk_acc,walk_dec)(=0.01),
        (float)(walk_speed_x)(=1),
        (float)(jump_speed)(=1),
        (int)(jump_ticks)(=4),
    ))

    Reflect(ConfigData)
    (
        (Global)(global),
        (Camera)(camera),
        (Humans)(humans),
    )

    static void DisplayGui();
};

extern const ConfigData &config;
