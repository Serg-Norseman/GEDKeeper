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
        private class PersonalEvent
        {
            public readonly GEDCOMIndividualRecord IRec;
            public readonly GEDCOMCustomEvent Event;
            public readonly UDN Date;

            public PersonalEvent(GEDCOMIndividualRecord iRec, GEDCOMCustomEvent evt)
            {
                IRec = iRec;
                Event = evt;
                Date = (evt == null) ? UDN.CreateEmpty() : evt.Date.GetUDN();
            }
        }

        private readonly GEDCOMIndividualRecord fPerson;
        private IFont fTitleFont, fChapFont, fTextFont;

        public PersonalEventsReport(IBaseWindow baseWin, GEDCOMIndividualRecord selectedPerson)
            : base(baseWin)
        {
            fTitle = SRLangMan.LS(RLS.LSID_PER_Title);
            fPerson = selectedPerson;
        }

        // TODO: output marriage and child events
        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
            IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

            fTitleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            fChapFont = fWriter.CreateFont("", 16f, true, false, clrBlack);
            fTextFont = fWriter.CreateFont("", 10f, false, false, clrBlack);

            fWriter.addParagraph(fTitle, fTitleFont, CustomWriter.TextAlignment.taLeft);
            fWriter.addParagraph(GKUtils.GetNameString(fPerson, true, false), fTitleFont, CustomWriter.TextAlignment.taLeft);

            var evList = new List<PersonalEvent>();
            int num = fPerson.Events.Count;
            for (int i = 0; i < num; i++) {
                GEDCOMCustomEvent evt = fPerson.Events[i];
                if (evt.GetChronologicalYear() == 0) continue;

                evList.Add(new PersonalEvent(fPerson, evt));
            }
            SortHelper.QuickSort(evList, EventsCompare);

            fWriter.beginList();

            //int birthYear = -1;

            int num4 = evList.Count;
            for (int i = 0; i < num4; i++) {
                PersonalEvent evObj = evList[i];
                if (!evObj.Date.HasKnownYear()) continue;

                GEDCOMCustomEvent evt = evObj.Event;

                string li;

                if (evObj.IRec == fPerson) {
                    int year = evt.GetChronologicalYear();
                    int age = GKUtils.GetAge(fPerson, year);

                    int ev = GKUtils.GetPersonEventIndex(evt.Name);
                    string st;
                    if (ev == 0) {
                        st = evt.Classification;
                    } else {
                        st = (ev > 0) ? LangMan.LS(GKData.PersonEvents[ev].Name) : evt.Name;
                    }

                    string dt = GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);
                    if (/*ShowAges*/true) {
                        dt += string.Format(" ({0})", age);
                    }

                    li = dt + ": " + st + ".";
                    if (evt.Place.StringValue != "") {
                        li = li + " " + LangMan.LS(LSID.LSID_Place) + ": " + evt.Place.StringValue;
                    }

                    fWriter.addListItem(" " + li, fTextFont);
                }
                /*else
                {
                    string dt = (evt == null) ? "?" : GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false);

                    string st = (evObj.IRec.Sex == GEDCOMSex.svMale) ? ": Родился " : ": Родилась ";

                    li = dt + st + GKUtils.GetNameString(evObj.IRec, true, false);
                    PedigreePerson prs = FindPerson(evObj.IRec);
                    string id = (prs != null) ? prs.Id : "";

                    fWriter.addListItemLink(" " + li + " ", fTextFont, id, fLinkFont);
                }*/
            }

            fWriter.endList();
        }

        private static int EventsCompare(PersonalEvent item1, PersonalEvent item2)
        {
            return item1.Date.CompareTo(item2.Date);
        }
    }
}
