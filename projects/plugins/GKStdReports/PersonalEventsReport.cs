/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2025 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore;
using GKCore.Calendar;
using GKCore.Design.Graphics;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKStdReports
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PersonalEventsReport : ReportExporter
    {
        private enum EventType
        {
            Personal,
            Parent,
            Spouse,
            Child
        }

        private class PersonalEvent
        {
            public readonly EventType Type;
            public readonly GDMRecord Rec;
            public readonly GDMCustomEvent Event;
            public readonly UDN Date;

            public PersonalEvent(EventType type, GDMRecord rec, GDMCustomEvent evt)
            {
                Type = type;
                Rec = rec;
                Event = evt;
                Date = (evt == null) ? UDN.Unknown : evt.Date.GetUDN();
            }
        }

        private readonly GDMIndividualRecord fPerson;

        public bool ShowAges = true;

        public PersonalEventsReport(IBaseWindow baseWin, GDMIndividualRecord selectedPerson)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(PLS.PersonalEventsReport);
            fPerson = selectedPerson;
        }

        private static void ExtractEvents(EventType type, IList<PersonalEvent> list, IGDMRecordWithEvents record)
        {
            if (!record.HasEvents) return;

            int num = record.Events.Count;
            for (int i = 0; i < num; i++) {
                GDMCustomEvent evt = record.Events[i];
                if (evt.GetChronologicalYear() != 0) {
                    list.Add(new PersonalEvent(type, (GDMRecord)record, evt));
                }
            }
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);

            var titleFont = fWriter.CreateFont("", 14f, true, false, clrBlack);
            var textFont = fWriter.CreateFont("", 10f, false, false, clrBlack);

            fWriter.AddParagraph(GKUtils.GetNameString(fPerson, true, false), titleFont, TextAlignment.taLeft);
            fWriter.NewLine();

            IBaseContext baseContext = fBase.Context;

            var evList = new List<PersonalEvent>();

            GDMIndividualRecord father = null, mother = null;
            GDMFamilyRecord parFamily = baseContext.Tree.GetParentsFamily(fPerson);
            if (parFamily != null && baseContext.IsRecordAccess(parFamily.Restriction)) {
                baseContext.Tree.GetSpouses(parFamily, out father, out mother);
            }

            ExtractEvents(EventType.Personal, evList, fPerson);

            int num2 = fPerson.SpouseToFamilyLinks.Count;
            for (int j = 0; j < num2; j++) {
                GDMFamilyRecord family = baseContext.Tree.GetPtrValue(fPerson.SpouseToFamilyLinks[j]);
                if (!baseContext.IsRecordAccess(family.Restriction))
                    continue;

                ExtractEvents(EventType.Spouse, evList, family);

                int num3 = family.Children.Count;
                for (int i = 0; i < num3; i++) {
                    GDMIndividualRecord child = baseContext.Tree.GetPtrValue(family.Children[i]);
                    GDMCustomEvent evt = child.FindEvent(GEDCOMTagType.BIRT);
                    if (evt != null && evt.GetChronologicalYear() != 0) {
                        evList.Add(new PersonalEvent(EventType.Child, child, evt));
                    }
                }
            }

            SortHelper.QuickSort(evList, EventsCompare);
            fWriter.BeginList();

            int num4 = evList.Count;
            for (int i = 0; i < num4; i++) {
                PersonalEvent evObj = evList[i];
                if (!evObj.Date.HasKnownYear()) continue;

                GDMCustomEvent evt = evObj.Event;
                string st = GKUtils.GetEventName(evt);
                string dt = GKUtils.GEDCOMEventToDateStr(evt, GlobalOptions.Instance.DefDateFormat, false);

                if (ShowAges) {
                    int year = evt.GetChronologicalYear();
                    int age = (evObj.Rec == fPerson) ? GKUtils.GetAge(fPerson, year) : -1;
                    if (age >= 0) {
                        dt += string.Format(" ({0})", age);
                    }
                }

                string li = dt + ": " + st + ".";
                if (evt.HasPlace && evt.Place.StringValue != "") {
                    li = li + " " + LangMan.LS(LSID.Place) + ": " + evt.Place.StringValue;
                }
                fWriter.AddListItem("   " + li, textFont);

                if (evObj.Rec is GDMIndividualRecord) {
                    GDMIndividualRecord iRec = (GDMIndividualRecord)evObj.Rec;

                    if (evt.GetTagType() == GEDCOMTagType.BIRT) {
                        if (evObj.Type == EventType.Personal) {
                            if (father != null) {
                                fWriter.AddListItem("   " + "   " + LangMan.LS(LSID.Father) + ": " + GKUtils.GetNameString(father, true, false) + " ", textFont);
                            }
                            if (mother != null) {
                                fWriter.AddListItem("   " + "   " + LangMan.LS(LSID.Mother) + ": " + GKUtils.GetNameString(mother, true, false) + " ", textFont);
                            }
                        } else if (evObj.Type == EventType.Child) {
                            if (iRec.Sex == GDMSex.svMale) {
                                st = LangMan.LS(LSID.RK_Son) + ": ";
                            } else {
                                st = LangMan.LS(LSID.RK_Daughter) + ": ";
                            }
                            st = StringHelper.UniformName(st) + GKUtils.GetNameString(iRec, true, false);
                            fWriter.AddListItem("   " + "   " + st, textFont);
                        }
                    }
                } else if (evObj.Rec is GDMFamilyRecord) {
                    GDMFamilyRecord famRec = (GDMFamilyRecord)evObj.Rec;

                    GDMIndividualRecord sp;
                    string unk;
                    if (fPerson.Sex == GDMSex.svMale) {
                        sp = baseContext.Tree.GetPtrValue(famRec.Wife);
                        st = LangMan.LS(LSID.Wife) + ": ";
                        unk = LangMan.LS(LSID.UnkFemale);
                    } else {
                        sp = baseContext.Tree.GetPtrValue(famRec.Husband);
                        st = LangMan.LS(LSID.Husband) + ": ";
                        unk = LangMan.LS(LSID.UnkMale);
                    }

                    string sps;
                    if (sp != null) {
                        sps = st + GKUtils.GetNameString(sp, true, false)/* + GKUtils.GetPedigreeLifeStr(sp, fOptions.PedigreeOptions.Format)*/;
                    } else {
                        sps = st + unk;
                    }
                    fWriter.AddListItem("   " + "   " + sps, textFont);
                }
            }

            fWriter.EndList();
        }

        private static int EventsCompare(PersonalEvent item1, PersonalEvent item2)
        {
            return item1.Date.CompareTo(item2.Date);
        }
    }
}
