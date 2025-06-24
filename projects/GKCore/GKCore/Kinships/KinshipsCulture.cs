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
using System.Collections.Generic;

namespace GKCore.Kinships
{
    internal sealed class KSTypeEntry
    {
        public int Index;
        public string Sign;
        public string Name;
        public bool HasExt;
        public string Description;

        public KSTypeEntry()
        {
        }

        public override string ToString()
        {
            return $"{Index} / {Sign} / {Name} / {HasExt} / {Description}";
        }

        public void Prepare()
        {
            Name = KinshipsCulture.GetLocalizedString(Name);
        }
    }


    internal sealed class KSDefinitionEntry
    {
        public int Index;
        public bool Enable;

        public string[] PrevPrevious;
        public string[] Previous;
        public string[] Current;
        public string SpecConditions;
        public string Result;

        public short Great;
        public short Degree;

        public HashSet<int> PrevPrevRels;
        public HashSet<int> PrevRels;
        public HashSet<int> CurrRels;
        public int FinRel;


        public KSDefinitionEntry()
        {
            PrevPrevRels = new HashSet<int>();
            PrevRels = new HashSet<int>();
            CurrRels = new HashSet<int>();
            FinRel = 0;
        }

        public override string ToString()
        {
            return $"{Index} / {Enable} / [{string.Join(",", PrevPrevious)}] / [{string.Join(",", Previous)}] / [{string.Join(",", Current)}] / {Result} / {Great} / {Degree}";
        }

        public void Prepare(Dictionary<string, int> indexes)
        {
            ProcessTypes(indexes, PrevPrevious, PrevPrevRels);
            ProcessTypes(indexes, Previous, PrevRels);
            ProcessTypes(indexes, Current, CurrRels);

            if (indexes.TryGetValue(Result, out int idx)) {
                FinRel = idx;
            } else {
                Logger.WriteError($"KSDefinitionEntry.Prepare({Index}): Type signature `{Result}` does not match index");
            }
        }

        private void ProcessTypes(Dictionary<string, int> indexes, string[] array, HashSet<int> hashSet)
        {
            foreach (var str in array) {
                if (indexes.TryGetValue(str, out int idx)) {
                    hashSet.Add(idx);
                } else {
                    Logger.WriteError($"KSDefinitionEntry.Prepare({Index}): Type signature `{str}` does not match index");
                }
            }
        }
    }


    /// <summary>
    /// Config for the culture of kinships.
    /// </summary>
    internal class KinshipsCulture
    {
        public string GreatPrefix { get; set; }

        public KSTypeEntry[] Types { get; set; }
        public KSDefinitionEntry[] Definitions { get; set; }

        public int[] KinshipTypeIndexes { get; set; }


        public KinshipsCulture()
        {
            Types = new KSTypeEntry[0];
            Definitions = new KSDefinitionEntry[0];
        }

        public KSTypeEntry GetTypeEntry(int id)
        {
            int index = KinshipTypeIndexes[id];
            return Types[index];
        }

        public void Prepare()
        {
            GreatPrefix = GetLocalizedString(GreatPrefix);

            KinshipTypeIndexes = new int[(int)KinshipType.ktLast + 1];
            for (KinshipType kt = KinshipType.ktNone; kt < KinshipType.ktLast; kt++) {
                var sign = kt.ToString();
                var index = Array.FindIndex(Types, (kte) => kte.Sign == sign);
                var ktEntry = Types[index];
                KinshipTypeIndexes[(int)kt] = index;
            }

            var indexes = new Dictionary<string, int>();
            foreach (var kinType in Types) {
                indexes.Add(kinType.Sign, kinType.Index);
                kinType.Prepare();
            }

            foreach (var kinDef in Definitions) {
                kinDef.Prepare(indexes);
            }
        }

        public static string GetLocalizedString(string value)
        {
            if (value.StartsWith("@")) {
                int lsid = int.Parse(value.Substring(1));
                value = LangMan.LS((LSID)lsid);
            }
            return value;
        }
    }
}
