#include "image_data.h"

Images::Images(const TextureAtlas &atlas)
{
    auto refl = Refl::Interface<Images>(*this);
    refl.for_each_field([&](auto index)
    {
        constexpr int i = index.value;
        refl.template field_value<i>() = atlas.Get(refl.field_name(i) + ".png");
    });
}
