/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Yaml;
using System.Yaml.Serialization;

namespace GKCommon
{
    public static class YamlHelper
    {
        private static YamlSerializer serializer;

        static YamlHelper()
        {
            var config = new YamlConfig();
            config.DontUseVerbatimTag = true;
            config.OmitTagForRootNode = true;
            config.ExcludeYamlVersion = true;
            serializer = new YamlSerializer(config);
        }

        public static string Serialize(object target)
        {
            return serializer.Serialize(target);
        }

        public static object Deserialize(string value)
        {
            return serializer.Deserialize(value)[0];
        }

        public static object[] Deserialize(string value, params Type[] types)
        {
            return serializer.Deserialize(value, types);
        }
    }
}
