#pragma once

#include "gameutils/texture_atlas.h"
#include "reflection/complete.h"

struct Images
{
    Reflect(Images)
    (
        (TextureAtlas::Image)(tiles,humans,effects,font,dialogue,bg1,darkness),
    )

    Images() {}
    Images(const TextureAtlas &atlas);
};
