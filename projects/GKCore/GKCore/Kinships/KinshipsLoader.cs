/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

namespace GKCore.Kinships
{
    public sealed class KinshipType
    {
        public string Sign;
        public string Name;
        public bool HasExt;
        public string Description;

        public KinshipType()
        {
        }

        public override string ToString()
        {
            return $"{Sign} / {Name} / {HasExt} / {Description}";
        }
    }


    public sealed class KinshipDefinition
    {
        public string[] PrevPrevious;
        public string[] Previous;
        public string[] Current;
        public string Result;

        public short Great;
        public short Degree;

        public KinshipDefinition()
        {
        }

        public override string ToString()
        {
            return $"[{string.Join(",", PrevPrevious)}] / [{string.Join(",", Previous)}] / [{string.Join(",", Current)}] / {Result} / {Great} / {Degree}";
        }
    }


    public class KinshipsCulture
    {
        public KinshipType[] Types { get; set; }
        public KinshipDefinition[] Definitions { get; set; }

        public KinshipsCulture()
        {
            Types = new KinshipType[0];
            Definitions = new KinshipDefinition[0];
        }
    }


    public class KinshipsLoader
    {
        private static KinshipsCulture fKinshipsCulture;

        public static KinshipsCulture KinshipsCulture
        {
            get { return fKinshipsCulture; }
        }

        static KinshipsLoader()
        {
            fKinshipsCulture = new KinshipsCulture();
        }

        public static void Load(string fileName)
        {
            if (!File.Exists(fileName)) return;

            try {
                // loading file
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    fKinshipsCulture = YamlHelper.Deserialize<KinshipsCulture>(content);
                }

                // processing
            } catch (Exception ex) {
                Logger.WriteError("KinshipsLoader.Load()", ex);
            }
        }

        // WARN: need for static initialization if no culture files!
        /*private static string[] GetRels(EnumSet<RelationKind> enumSet)
        {
            var result = new List<string>();
            for (RelationKind relKind = RelationKind.rkNone; relKind < RelationKind.rkLast; relKind++) {
                if (enumSet.Contains(relKind))
                    result.Add(relKind.ToString());
            }
            return result.ToArray();
        }

        public void Save(string fileName)
        {
            fKinshipsCulture.Types = new KinshipType[(int)RelationKind.rkLast + 1];
            for (RelationKind relKind = RelationKind.rkNone; relKind < RelationKind.rkLast; relKind++) {
                var internRelKind = KinshipsMan.RelationKinds[(int)relKind];

                var kinType = new KinshipType() {
                    Sign = relKind.ToString(),
                    Name = $"@{(int)internRelKind.Name}",
                    HasExt = internRelKind.HasExt,
                    Description = LangMan.LS(internRelKind.Name)
                };

                fKinshipsCulture.Types[(int)relKind] = kinType;
            }

            fKinshipsCulture.Definitions = new KinshipDefinition[KinshipsMan.Kinships.Count];
            for (int i = 0; i < KinshipsMan.Kinships.Count; i++) {
                var kin = KinshipsMan.Kinships[i];

                var kinDef = new KinshipDefinition() {
                    PrevPrevious = GetRels(kin.PrevPrevRels),
                    Previous = GetRels(kin.PrevRels),
                    Current = GetRels(kin.CurrRels),
                    Result = kin.FinRel.ToString(),
                    Great = kin.Great,
                    Degree = kin.Degree,
                };

                fKinshipsCulture.Definitions[i] = kinDef;
            }

            try {
                if (File.Exists(fileName))
                    File.Delete(fileName);

                using (var writer = new StreamWriter(fileName, false, Encoding.UTF8)) {
                    writer.Write(YamlHelper.Serialize(fKinshipsCulture));
                }
            } catch (Exception ex) {
                Logger.WriteError("KinshipsLoader.Save()", ex);
            }
        }*/
    }
}
