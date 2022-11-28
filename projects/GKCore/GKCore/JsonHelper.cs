/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Serialization;

namespace GKCore
{
    public static class JsonHelper
    {
        private static JsonSerializerSettings fSerializerSettings;

        private static JsonSerializerSettings SerializerSettings
        {
            get {
                if (fSerializerSettings == null) {
                    var converters = new List<JsonConverter> { new StringEnumConverter { NamingStrategy = null } };
                    var resolver = new DefaultContractResolver();
                    fSerializerSettings = new JsonSerializerSettings {
                        ContractResolver = resolver,
                        Converters = converters,
                        NullValueHandling = NullValueHandling.Ignore
                    };
                }
                return fSerializerSettings;
            }
        }


        public static string Serialize(object target)
        {
            return JsonConvert.SerializeObject(target, Formatting.Indented, SerializerSettings);
        }

        public static T Deserialize<T>(string value)
        {
            return JsonConvert.DeserializeObject<T>(value, SerializerSettings);
        }
    }
}
