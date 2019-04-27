#pragma once

#include "gameutils/texture_atlas.h"
#include "reflection/complete.h"

struct Images
{
    Reflect(Images)
    (
        (TextureAtlas::Image)(tiles,human),
    )

    Images(const TextureAtlas &atlas);
};
