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
using System.IO;
using GDModel;
using GKCore.Options;

namespace GKCore.Kinships
{
    /// <summary>
    /// 
    /// </summary>
    public static class KinshipsMan
    {
        public static readonly LSID GreatPrefix;
        public static readonly LSID[] KinDegrees;
        public static readonly LSID[] KinExts;

        private static KinshipsCulture fKinshipsCulture;


        static KinshipsMan()
        {
            fKinshipsCulture = new KinshipsCulture();

            GreatPrefix = LSID.RK_GreatPrefix;

            KinDegrees = new LSID[] {
                LSID.KinshipDegree_01,
                LSID.KinshipDegree_02,
                LSID.KinshipDegree_03,
                LSID.KinshipDegree_04,
                LSID.KinshipDegree_05,
                LSID.KinshipDegree_06,
                LSID.KinshipDegree_07,
                LSID.KinshipDegree_08,
                LSID.KinshipDegree_09,
                LSID.KinshipDegree_10,
            };

            KinExts = new LSID[] {
                LSID.None,
                LSID.RE_Blood,
                LSID.RE_Uterine,
                LSID.None,
                LSID.RE_Adoptive,
                LSID.RE_Adopted,
                LSID.RE_CommonLaw,
            };
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
                for (KinshipType kt = KinshipType.ktNone; kt < KinshipType.ktLast; kt++) {
                    var sign = kt.ToString();
                    var index = Array.FindIndex(fKinshipsCulture.Types, (kte) => kte.Sign == sign);
                    var ktEntry = fKinshipsCulture.Types[index];
                    ktEntry.Id = (int)kt;
                    fKinshipsCulture.KinshipTypeIndexes[(int)kt] = index;
                }

                var indexes = new Dictionary<string, int>();
                foreach (var kinType in fKinshipsCulture.Types) {
                    indexes.Add(kinType.Sign, kinType.Id);
                    kinType.Prepare();
                }

                foreach (var kinDef in fKinshipsCulture.Definitions) {
                    kinDef.Prepare(indexes);
                }
            } catch (Exception ex) {
                Logger.WriteError("KinshipsLoader.Load()", ex);
            }
        }

        public static int FindKinship(int prevprev, int prev, int cur, out int great, out int degree)
        {
            int finRel = (int)KinshipType.ktUndefined;
            great = 0;
            degree = 0;

            var kinshipDefs = fKinshipsCulture.Definitions;
            for (int i = 0, num = kinshipDefs.Length; i < num; i++) {
                var kinDef = kinshipDefs[i];

                if (kinDef.PrevRels.Contains(prev) && kinDef.CurrRels.Contains(cur) && (kinDef.PrevPrevRels.Count == 0 || kinDef.PrevPrevRels.Contains(prevprev))) {
                    great = kinDef.Great;
                    degree = kinDef.Degree;
                    finRel = (kinDef.FinRel == (int)KinshipType.ktSame) ? cur : kinDef.FinRel;
                    break;
                }
            }

            return finRel;
        }

        public static KinshipType FixLink(GDMSex targetSex, KinshipType rel)
        {
            KinshipType resRel = rel;

            switch (rel) {
                case KinshipType.ktParent:
                    switch (targetSex) {
                        case GDMSex.svMale:
                            resRel = KinshipType.ktFather;
                            break;
                        case GDMSex.svFemale:
                            resRel = KinshipType.ktMother;
                            break;
                    }
                    break;

                case KinshipType.ktSpouse:
                    switch (targetSex) {
                        case GDMSex.svMale:
                            resRel = KinshipType.ktHusband;
                            break;
                        case GDMSex.svFemale:
                            resRel = KinshipType.ktWife;
                            break;
                    }
                    break;

                case KinshipType.ktChild:
                    switch (targetSex) {
                        case GDMSex.svMale:
                            resRel = KinshipType.ktSon;
                            break;
                        case GDMSex.svFemale:
                            resRel = KinshipType.ktDaughter;
                            break;
                    }
                    break;

                default:
                    resRel = rel;
                    break;
            }

            return resRel;
        }

        public static string GetExt(int n, GDMSex sex)
        {
            string result = (n == 0 || n == 3) ? string.Empty : (LangMan.LSS(KinExts[n], (int)sex - 1) + " ");
            return result;
        }

        public static string GetDegree(int n, GDMSex sex)
        {
            string result = (n == 0) ? string.Empty : (LangMan.LSS(KinDegrees[n], (int)sex - 1) + " ");
            return result;
        }

        public static string GetGreat(int n, bool shortForm)
        {
            string result = "";
            if (n > 0) {
                if (!shortForm) {
                    for (int i = 1; i <= n; i++) {
                        result += LangMan.LS(GreatPrefix);
                    }
                } else {
                    result = LangMan.LS(GreatPrefix);
                    if (n > 1) {
                        result += string.Format("({0})", n);
                    }
                }
            }
            return result;
        }

        public static string GetRelationName(GDMIndividualRecord target, int rel, int great, int degree, bool shortForm, KinshipExt ext)
        {
            string tmp = string.Empty;

            if (rel != (int)KinshipType.ktUndefined) {
                if (degree == 1) {
                    if (rel == (int)KinshipType.ktSister) {
                        rel = (int)KinshipType.ktCousinF;
                        degree = 0;
                    }
                    if (rel == (int)KinshipType.ktBrother) {
                        rel = (int)KinshipType.ktCousinM;
                        degree = 0;
                    }
                }

                tmp = GetDegree(degree, target.Sex);
                tmp += GetGreat(great, shortForm);
            }

            var kinTypeEntry = fKinshipsCulture.GetTypeEntry((int)rel);

            if (GlobalOptions.Instance.ExtendedKinships && kinTypeEntry.HasExt) {
                tmp = GetExt((int)ext, target.Sex) + tmp;
            }

            return tmp + kinTypeEntry.Name;
        }
    }
}
