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
            //config.ExcludeYamlVersion
            serializer = new YamlSerializer(config);
        }

        public static string SerializeObject(object target)
        {
            return serializer.Serialize(target);
        }

        public static object DeserializeObject(string value)
        {
            return serializer.Deserialize(value)[0];
        }

        public static object[] DeserializeObject(string value, params Type[] types)
        {
            return serializer.Deserialize(value, types);
        }
    }
}