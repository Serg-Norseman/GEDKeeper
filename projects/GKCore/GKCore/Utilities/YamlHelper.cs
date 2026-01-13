/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using YamlDotNet.Serialization;

namespace GKCore.Utilities
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
