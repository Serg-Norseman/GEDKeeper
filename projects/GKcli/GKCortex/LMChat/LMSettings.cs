/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;

namespace GKCortex.LMChat;

public sealed class LMSettings
{
    public string APIAddress { get; set; }
    public string APIKey { get; set; }

    public string ModelId { get; set; }

    public bool StreamMode { get; set; }

    #region Model parameters

    public double Temperature { get; set; } = 0.7;
    public double TopP { get; set; } = 0.9;
    public double PresencePenalty { get; set; } = 0.0;
    public double FrequencyPenalty { get; set; } = 0.0;
    public int MaxTokens { get; set; } = 2048;
    public bool ReasoningMode { get; set; } = true;

    #endregion


    public void LoadOptions(IniFile ini, string section)
    {
        APIAddress = ini.ReadString(section, "APIAddress", "http://127.0.0.1:1337/");
        APIKey = ini.ReadString(section, "APIKey", string.Empty);

        ModelId = ini.ReadString(section, "ModelId", string.Empty);
        StreamMode = ini.ReadBool(section, "StreamMode", false);

        Temperature = ini.ReadFloat(section, "Temperature", 0.7);
        TopP = ini.ReadFloat(section, "TopP", 0.9);
        PresencePenalty = ini.ReadFloat(section, "PresencePenalty", 0.0);
        FrequencyPenalty = ini.ReadFloat(section, "FrequencyPenalty", 0.0);
        MaxTokens = ini.ReadInteger(section, "MaxTokens", 2048);
        ReasoningMode = ini.ReadBool(section, "ReasoningMode", true);
    }

    public void SaveOptions(IniFile ini, string section)
    {
        ini.WriteString(section, "APIAddress", APIAddress);
        ini.WriteString(section, "APIKey", APIKey);

        ini.WriteString(section, "ModelId", ModelId);
        ini.WriteBool(section, "StreamMode", StreamMode);

        ini.WriteFloat(section, "Temperature", Temperature);
        ini.WriteFloat(section, "TopP", TopP);
        ini.WriteFloat(section, "PresencePenalty", PresencePenalty);
        ini.WriteFloat(section, "FrequencyPenalty", FrequencyPenalty);
        ini.WriteInteger(section, "MaxTokens", MaxTokens);
        ini.WriteBool(section, "ReasoningMode", ReasoningMode);
    }
}
