/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2022 by Sergey V. Zhdanovskih.
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
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

namespace GKCore
{
    /// <summary>
    /// This class is a helper for serializing/deserializing data in yaml format.
    /// </summary>
    public static class YamlHelper
    {
        private static readonly ISerializer serializer;
        private static readonly IDeserializer deserializer;

        static YamlHelper()
        {
            serializer = new SerializerBuilder().Build();
            //config.DontUseVerbatimTag = true;
            //config.OmitTagForRootNode = true;
            //config.EmitYamlVersion = false;

            deserializer = new DeserializerBuilder().Build();
        }

        public static string Serialize(object target)
        {
            return serializer.Serialize(target);
        }

        public static T Deserialize<T>(string value)
        {
            return deserializer.Deserialize<T>(value);
        }
    }
}
