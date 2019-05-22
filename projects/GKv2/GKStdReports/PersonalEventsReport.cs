/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018 by Sergey V. Zhdanovskih.
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
using BSLib;
using BSLib.Calendar;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Types;

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
                Date = (evt == null) ? UDN.CreateEmpty() : evt.Date.GetUDN();
            }
        }

        private readonly GDMIndividualRecord fPerson;
        private IFont fTitleFont, fChapFont, fTextFont;

        public bool ShowAges = true;

        public PersonalEventsReport(IBaseWindow baseWin, GDMIndividualRecord selectedPerson)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(RLS.LSID_PER_Title);
            fPerson = selectedPerson;
        }

        private void ExtractEvents(EventType type, List<PersonalEvent> list, IGEDCOMRecordWithEvents record)
        {
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
            IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

            fTitleFont = fWriter.CreateFont("", 14f, true, false, clrBlack);
            fChapFont = fWriter.CreateFont("", 12f, true, false, clrBlack);
            fTextFont = fWriter.CreateFont("", 10f, false, false, clrBlack);

            //fWriter.AddParagraph(fTitle, fTitleFont, TextAlignment.taLeft);
            fWriter.AddParagraph(GKUtils.GetNameString(fPerson, true, false), fTitleFont, TextAlignment.taLeft);
            fWriter.NewLine();

            var evList = new List<PersonalEvent>();

            GDMIndividualRecord father = null, mother = null;
            if (fPerson.ChildToFamilyLinks.Count > 0) {
                GDMFamilyRecord family = fPerson.ChildToFamilyLinks[0].Family;
                if (fBase.Context.IsRecordAccess(family.Restriction)) {
                    father = family.GetHusband();
                    mother = family.GetWife();
                }
            }

            ExtractEvents(EventType.Personal, evList, fPerson);

            int num2 = fPerson.SpouseToFamilyLinks.Count;
            for (int j = 0; j < num2; j++) {
                GDMFamilyRecord family = fPerson.SpouseToFamilyLinks[j].Family;
                if (!fBase.Context.IsRecordAccess(family.Restriction))
                    continue;

                ExtractEvents(EventType.Spouse, evList, family);

                int num3 = family.Children.Count;
                for (int i = 0; i < num3; i++) {
                    GDMIndividualRecord child = family.Children[i].Value as GDMIndividualRecord;
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
                string dt = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);

                if (ShowAges) {
                    int year = evt.GetChronologicalYear();
                    int age = (evObj.Rec == fPerson) ? GKUtils.GetAge(fPerson, year) : -1;
                    if (age >= 0) {
                        dt += string.Format(" ({0})", age);
                    }
                }

                string li = dt + ": " + st + ".";
                if (evt.Place.StringValue != "") {
                    li = li + " " + LangMan.LS(LSID.LSID_Place) + ": " + evt.Place.StringValue;
                }
                fWriter.AddListItem("   " + li, fTextFont);

                if (evObj.Rec is GDMIndividualRecord) {
                    GDMIndividualRecord iRec = evObj.Rec as GDMIndividualRecord;

                    if (evt.Name == GEDCOMTagType.BIRT) {
                        if (evObj.Type == EventType.Personal) {
                            if (father != null) {
                                fWriter.AddListItem("   " + "   " + LangMan.LS(LSID.LSID_Father) + ": " + GKUtils.GetNameString(father, true, false) + " ", fTextFont);
                            }
                            if (mother != null) {
                                fWriter.AddListItem("   " + "   " + LangMan.LS(LSID.LSID_Mother) + ": " + GKUtils.GetNameString(mother, true, false) + " ", fTextFont);
                            }
                        } else if (evObj.Type == EventType.Child) {
                            if (iRec.Sex == GEDCOMSex.svMale) {
                                st = LangMan.LS(LSID.LSID_RK_Son) + ": ";
                            } else {
                                st = LangMan.LS(LSID.LSID_RK_Daughter) + ": ";
                            }
                            st = ConvertHelper.UniformName(st) + GKUtils.GetNameString(iRec, true, false);
                            fWriter.AddListItem("   " + "   " + st, fTextFont);
                        }
                    }
                } else if (evObj.Rec is GDMFamilyRecord) {
                    GDMFamilyRecord famRec = evObj.Rec as GDMFamilyRecord;

                    GDMIndividualRecord sp;
                    string unk;
                    if (fPerson.Sex == GEDCOMSex.svMale) {
                        sp = famRec.GetWife();
                        st = LangMan.LS(LSID.LSID_Wife) + ": ";
                        unk = LangMan.LS(LSID.LSID_UnkFemale);
                    } else {
                        sp = famRec.GetHusband();
                        st = LangMan.LS(LSID.LSID_Husband) + ": ";
                        unk = LangMan.LS(LSID.LSID_UnkMale);
                    }

                    string sps;
                    if (sp != null) {
                        sps = st + GKUtils.GetNameString(sp, true, false)/* + GKUtils.GetPedigreeLifeStr(sp, fOptions.PedigreeOptions.Format)*/;
                    } else {
                        sps = st + unk;
                    }
                    fWriter.AddListItem("   " + "   " + sps, fTextFont);
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
