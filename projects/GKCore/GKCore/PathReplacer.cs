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
using System.IO;
using GKCommon;

namespace GKCore
{
    public sealed class PathsMapping
    {
        public string Source;
        public string Target;
    }

    internal class PathsMappingsList
    {
        public PathsMapping[] PathsMappings { get; set; }

        public PathsMappingsList()
        {
            PathsMappings = new PathsMapping[0];
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class PathReplacer
    {
        private PathsMappingsList fPathsMappings;

        public PathReplacer()
        {
            fPathsMappings = new PathsMappingsList();
        }

        public void Load(string fileName)
        {
            if (!File.Exists(fileName)) return;

            try
            {
                // loading database
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    var rawData = YamlHelper.Deserialize(content, typeof(PathsMappingsList));
                    fPathsMappings = rawData[0] as PathsMappingsList;
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PathReplacer.Load(): " + ex.Message);
            }
        }

        public string TryReplacePath(string path)
        {
            if (string.IsNullOrEmpty(path))
                throw new ArgumentNullException("path");

            try
            {
                for (int i = 0; i < fPathsMappings.PathsMappings.Length; i++) {
                    var pathsMapping = fPathsMappings.PathsMappings[i];

                    if (path.StartsWith(pathsMapping.Source)) {
                        string newPath = SysUtils.NormalizeFilename(path.Replace(pathsMapping.Source, pathsMapping.Target));
                        if (File.Exists(newPath)) {
                            return newPath;
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PathReplacer.TryReplacePath(): " + ex.Message);
            }

            return string.Empty;
        }
    }
}
