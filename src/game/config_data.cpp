#include "config_data.h"

#include "gameutils/config.h"

Config<ConfigData> config_file("assets/config.refl");
const ConfigData &config = *config_file;

void ConfigData::DisplayGui()
{
    config_file.DisplayGui();
}
