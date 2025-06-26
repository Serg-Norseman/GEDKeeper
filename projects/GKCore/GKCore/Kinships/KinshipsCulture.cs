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
    internal abstract class KSEntry
    {
        public int Index;


        protected void ProcessTypes(Dictionary<string, int> indexes, string[] array, HashSet<int> hashSet)
        {
            foreach (var str in array) {
                if (indexes.TryGetValue(str, out int idx)) {
                    hashSet.Add(idx);
                } else {
                    Logger.WriteError($"{this.GetType().Name}.Prepare({Index}): Type signature `{str}` does not match index");
                }
            }
        }

        protected void ProcessType(Dictionary<string, int> indexes, string str, out int result)
        {
            if (!indexes.TryGetValue(str, out result)) {
                Logger.WriteError($"{this.GetType().Name}.Prepare({Index}): Type signature `{str}` does not match index");
            }
        }
    }


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

        public KSTypeEntry(int index, string sign, string name, bool hasExt)
        {
            Index = index;
            Sign = sign;
            Name = name;
            HasExt = hasExt;
        }

        public override string ToString()
        {
            return $"{Index} / {Sign} / {Name} / {HasExt} / {Description}";
        }

        public void Prepare()
        {
            if (!string.IsNullOrEmpty(Name))
                Name = KinshipsCulture.GetLocalizedString(Name);
        }
    }


    internal sealed class KSDefinitionEntry : KSEntry
    {
        public bool Enable;

        public string[] PrePrevious;
        public string[] Previous;
        public string[] Current;
        public string SpecConditions;
        public string Result;

        public short Great;
        public short Degree;

        public HashSet<int> PrePrevRels;
        public HashSet<int> PrevRels;
        public HashSet<int> CurrRels;
        public int FinRel;


        public KSDefinitionEntry()
        {
            PrePrevRels = new HashSet<int>();
            PrevRels = new HashSet<int>();
            CurrRels = new HashSet<int>();
            FinRel = 0;
        }

        public KSDefinitionEntry(string[] prevPrevious, string[] previous, string[] current, string result, short great, short degree) : this()
        {
            Enable = true;
            PrePrevious = prevPrevious;
            Previous = previous;
            Current = current;
            Result = result;
            Great = great;
            Degree = degree;
        }

        public override string ToString()
        {
            return $"{Index} / {Enable} / [{string.Join(",", PrePrevious)}] / [{string.Join(",", Previous)}] / [{string.Join(",", Current)}] / {Result} / {Great} / {Degree}";
        }

        public void Prepare(Dictionary<string, int> indexes)
        {
            ProcessTypes(indexes, PrePrevious, PrePrevRels);
            ProcessTypes(indexes, Previous, PrevRels);
            ProcessTypes(indexes, Current, CurrRels);
            ProcessType(indexes, Result, out FinRel);
        }
    }


    internal sealed class KSSubstitutionEntry : KSEntry
    {
        public bool Enable;

        public string[] Current;
        public string SpecConditions;
        public string Result;

        public short Great;
        public short Degree;

        public HashSet<int> CurrRels;
        public int FinRel;


        public KSSubstitutionEntry()
        {
            CurrRels = new HashSet<int>();
            FinRel = 0;
        }

        public KSSubstitutionEntry(string[] current, string specCondition, string result, short great, short degree) : this()
        {
            Enable = true;
            Current = current;
            SpecConditions = specCondition;
            Result = result;
            Great = great;
            Degree = degree;
        }

        public override string ToString()
        {
            return $"{Index} / {Enable} / [{string.Join(",", Current)}] / {Result} / {Great} / {Degree}";
        }

        public void Prepare(Dictionary<string, int> indexes)
        {
            ProcessTypes(indexes, Current, CurrRels);
            ProcessType(indexes, Result, out FinRel);
        }
    }


    /// <summary>
    /// Config for the culture of kinships.
    /// </summary>
    internal class KinshipsCulture
    {
        public string GreatPrefix { get; set; }
        public bool GreatUsed { get; set; }

        public KSTypeEntry[] Types { get; set; }
        public KSDefinitionEntry[] Definitions { get; set; }
        public KSSubstitutionEntry[] Substitutions { get; set; }

        public Dictionary<int, int> KinshipTypeIndexes { get; set; }


        public KinshipsCulture()
        {
            Types = new KSTypeEntry[0];
            Definitions = new KSDefinitionEntry[0];
            Substitutions = new KSSubstitutionEntry[0];
        }

        public KSTypeEntry GetTypeEntry(int id)
        {
            int index = KinshipTypeIndexes[id];
            return Types[index];
        }

        public void Prepare()
        {
            GreatPrefix = GetLocalizedString(GreatPrefix);

            KinshipTypeIndexes = new Dictionary<int, int>((int)KinshipType.ktLast + 1);
            /*for (KinshipType kt = KinshipType.ktNone; kt < KinshipType.ktLast; kt++) {
                var sign = kt.ToString();
                var index = Array.FindIndex(Types, (kte) => kte.Sign == sign);
                var ktEntry = Types[index];
                KinshipTypeIndexes[(int)kt] = index;
            }*/

            var indexes = new Dictionary<string, int>();
            for (int i = 0; i < Types.Length; i++) {
                KSTypeEntry kinType = Types[i];
                indexes.Add(kinType.Sign, kinType.Index);
                kinType.Prepare();
                KinshipTypeIndexes[kinType.Index] = i;
            }

            foreach (var kinDef in Definitions) {
                kinDef.Prepare(indexes);
            }

            foreach (var kinSubst in Substitutions) {
                kinSubst.Prepare(indexes);
            }
        }

        public static string GetLocalizedString(string value)
        {
            if (!string.IsNullOrEmpty(value) && value.StartsWith("@")) {
                int lsid = int.Parse(value.Substring(1));
                value = LangMan.LS((LSID)lsid);
            }
            return value;
        }

        /// <summary>
        /// Basic set of settings for testing.
        /// </summary>
        public void InitDefaults()
        {
            GreatPrefix = LangMan.LS(LSID.RK_GreatPrefix);

            Types = new KSTypeEntry[] {
                new KSTypeEntry(00, "ktNone", "none", false),
                new KSTypeEntry(01, "ktUndefined", "undefined", false),
                new KSTypeEntry(02, "ktSame", "same", false),
                new KSTypeEntry(03, "ktParent", "parent", false),
                new KSTypeEntry(04, "ktSpouse", "spouse", false),
                new KSTypeEntry(05, "ktChild", "child", false),
                new KSTypeEntry(06, "ktFather", "father", true),
                new KSTypeEntry(07, "ktMother", "mother", true),
                new KSTypeEntry(08, "ktHusband", "husband", true),
                new KSTypeEntry(09, "ktWife", "wife", true),
                new KSTypeEntry(10, "ktSon", "son", true),
                new KSTypeEntry(11, "ktDaughter", "daughter", true),
                new KSTypeEntry(12, "ktGrandfather", "grandfather", false),
                new KSTypeEntry(13, "ktGrandmother", "grandmother", false),
                new KSTypeEntry(14, "ktGrandson", "grandson", false),
                new KSTypeEntry(15, "ktGranddaughter", "granddaughter", false),
                new KSTypeEntry(16, "ktBrother", "brother", true),
                new KSTypeEntry(17, "ktSister", "sister", true),
                new KSTypeEntry(18, "ktSonInLaw", "son-in-law", false),
                new KSTypeEntry(19, "ktDaughterInLaw", "daughter-in-law", false),
                new KSTypeEntry(20, "ktHusbandFather", "", false),
                new KSTypeEntry(21, "ktHusbandMother", "", false),
                new KSTypeEntry(22, "ktWifeFather", "", false),
                new KSTypeEntry(23, "ktWifeMother", "", false),
                new KSTypeEntry(24, "ktUncle_FatherBrother", "", false),
                new KSTypeEntry(25, "ktAunt_FatherSister", "", false),
                new KSTypeEntry(26, "ktUncle_MotherBrother", "", false),
                new KSTypeEntry(27, "ktAunt_MotherSister", "", false),
                new KSTypeEntry(28, "ktNephew", "nephew", false),
                new KSTypeEntry(29, "ktNiece", "niece", false),
                new KSTypeEntry(30, "ktCousinM", "cousin-m", false),
                new KSTypeEntry(31, "ktCousinF", "cousin-f", false),
                new KSTypeEntry(32, "ktBrotherInLaw_H", "", false),
                new KSTypeEntry(33, "ktSisterInLaw_H", "", false),
                new KSTypeEntry(34, "ktBrotherInLaw_W", "", false),
                new KSTypeEntry(35, "ktSisterInLaw_W", "", false),
                new KSTypeEntry(36, "ktStepfather", "", false),
                new KSTypeEntry(37, "ktStepmother", "", false),
                new KSTypeEntry(38, "ktFathersWife", "", false),
                new KSTypeEntry(39, "ktMothersHusband", "", false),
                new KSTypeEntry(40, "ktUncle_AuntHusband", "", false),
                new KSTypeEntry(41, "ktAunt_UncleWife", "", false),
            };

            Definitions = new KSDefinitionEntry[] {
                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktNone" },
                    new string[] {"ktFather", "ktMother", "ktHusband", "ktWife", "ktSon", "ktDaughter" },
                    "ktSame", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktHusband", "ktWife"},
                    new string[] {"ktSon", "ktDaughter"},
                    "ktSame", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktMother"},
                    new string[] {"ktHusband"},
                    "ktMothersHusband", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktFather"},
                    new string[] {"ktWife"},
                    "ktFathersWife", 0, 0),

                new KSDefinitionEntry(
                    new string[] {"ktFather"},
                    new string[] {"ktGrandfather", "ktGrandmother"},
                    new string[] {"ktSon"},
                    "ktUncle_FatherBrother", -1, 0),

                new KSDefinitionEntry(
                    new string[] {"ktFather"},
                    new string[] {"ktGrandfather", "ktGrandmother"},
                    new string[] {"ktDaughter"},
                    "ktAunt_FatherSister", -1, 0),

                new KSDefinitionEntry(
                    new string[] {"ktMother"},
                    new string[] {"ktGrandfather", "ktGrandmother"},
                    new string[] {"ktSon"},
                    "ktUncle_MotherBrother", -1, 0),

                new KSDefinitionEntry(
                    new string[] {"ktMother"},
                    new string[] {"ktGrandfather", "ktGrandmother"},
                    new string[] {"ktDaughter"},
                    "ktAunt_MotherSister", -1, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktAunt_FatherSister", "ktAunt_MotherSister"},
                    new string[] {"ktHusband"},
                    "ktUncle_AuntHusband", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktUncle_FatherBrother", "ktUncle_MotherBrother"},
                    new string[] {"ktWife"},
                    "ktAunt_UncleWife", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktBrother", "ktSister"},
                    new string[] {"ktSon"},
                    "ktNephew", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktBrother", "ktSister"},
                    new string[] {"ktDaughter"},
                    "ktNiece", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktSon", "ktBrother"},
                    new string[] {"ktWife"},
                    "ktDaughterInLaw", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktDaughter", "ktSister"},
                    new string[] {"ktHusband"},
                    "ktSonInLaw", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktWife"},
                    new string[] {"ktFather"},
                    "ktWifeFather", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktWife"},
                    new string[] {"ktMother"},
                    "ktWifeMother", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktHusband"},
                    new string[] {"ktFather"},
                    "ktHusbandFather", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktHusband"},
                    new string[] {"ktMother"},
                    "ktHusbandMother", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktFather", "ktMother"},
                    new string[] {"ktFather"},
                    "ktGrandfather", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktFather", "ktMother"},
                    new string[] {"ktMother"},
                    "ktGrandmother", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktGrandfather", "ktGrandmother"},
                    new string[] {"ktFather"},
                    "ktGrandfather", +1, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktGrandfather", "ktGrandmother"},
                    new string[] {"ktMother"},
                    "ktGrandmother", +1, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktFather", "ktMother"},
                    new string[] {"ktSon"},
                    "ktBrother", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktFather", "ktMother"},
                    new string[] {"ktDaughter"},
                    "ktSister", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktSon", "ktDaughter", "ktSonInLaw", "ktDaughterInLaw"},
                    new string[] {"ktSon"},
                    "ktGrandson", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktSon", "ktDaughter", "ktSonInLaw", "ktDaughterInLaw"},
                    new string[] {"ktDaughter"},
                    "ktGranddaughter", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktGrandson", "ktGranddaughter"},
                    new string[] {"ktSon"},
                    "ktGrandson", +1, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktGrandson", "ktGranddaughter"},
                    new string[] {"ktDaughter"},
                    "ktGranddaughter", +1, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktUncle_FatherBrother", "ktUncle_MotherBrother", "ktUncle_AuntHusband", "ktAunt_FatherSister", "ktAunt_MotherSister", "ktAunt_UncleWife"},
                    new string[] {"ktSon"},
                    "ktBrother", 0, +1),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktUncle_FatherBrother", "ktUncle_MotherBrother", "ktUncle_AuntHusband", "ktAunt_FatherSister", "ktAunt_MotherSister", "ktAunt_UncleWife"},
                    new string[] {"ktDaughter"},
                    "ktSister", 0, +1),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktNephew", "ktNiece"},
                    new string[] {"ktSon"},
                    "ktGrandson", +1, +1),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktNephew", "ktNiece"},
                    new string[] {"ktDaughter"},
                    "ktGranddaughter", +1, +1),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktHusband"},
                    new string[] {"ktBrother"},
                    "ktBrotherInLaw_H", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktWife"},
                    new string[] {"ktBrother"},
                    "ktBrotherInLaw_W", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktHusbandFather", "ktHusbandMother"},
                    new string[] {"ktSon"},
                    "ktBrotherInLaw_H", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktWifeFather", "ktWifeMother"},
                    new string[] {"ktSon"},
                    "ktBrotherInLaw_W", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktHusband"},
                    new string[] {"ktSister"},
                    "ktSisterInLaw_H", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktWife"},
                    new string[] {"ktSister"},
                    "ktSisterInLaw_W", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktHusbandFather", "ktHusbandMother"},
                    new string[] {"ktDaughter"},
                    "ktSisterInLaw_H", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktWifeFather", "ktWifeMother"},
                    new string[] {"ktDaughter"},
                    "ktSisterInLaw_W", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktBrotherInLaw_H", "ktBrotherInLaw_W", "ktSisterInLaw_H", "ktSisterInLaw_W"},
                    new string[] {"ktSon"},
                    "ktNephew", 0, 0),

                new KSDefinitionEntry(
                    new string[0],
                    new string[] {"ktBrotherInLaw_H", "ktBrotherInLaw_W", "ktSisterInLaw_H", "ktSisterInLaw_W"},
                    new string[] {"ktDaughter"},
                    "ktNiece", 0, 0),
            };

            Substitutions = new KSSubstitutionEntry[] {
                new KSSubstitutionEntry(new string[] {"ktSister" }, "D=1", "ktCousinF", 0, -1),
                new KSSubstitutionEntry(new string[] {"ktBrother"}, "D=1", "ktCousinM", 0, -1),
            };

            Prepare();
        }
    }
}
