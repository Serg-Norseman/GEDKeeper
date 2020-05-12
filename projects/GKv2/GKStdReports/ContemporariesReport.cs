﻿/*
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
using BSLib;
using BSLib.Design.Graphics;
using GDModel;
using GKCore;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Stats;

// FIXME: very bad algorithm
namespace GKStdReports
{
    /// <summary>
    /// 
    /// </summary>
    public class ContemporariesReport : ReportExporter
    {
        private readonly GDMIndividualRecord fPerson;
        private IFont fTitleFont, fChapFont, fTextFont;
        private CommonStats fStats;

        public ContemporariesReport(IBaseWindow baseWin, GDMIndividualRecord selectedPerson)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(RLS.LSID_Contemporaries_Title);
            fPerson = selectedPerson;
        }

        private Range<int> GetIndividualDates(GDMIndividualRecord iRec)
        {
            var dates = iRec.GetLifeDates();

            int yBirth = (dates.BirthEvent == null) ? 0 : dates.BirthEvent.GetChronologicalYear();
            int yDeath = (dates.DeathEvent == null) ? 0 : dates.DeathEvent.GetChronologicalYear();

            int provedLife = (iRec.Sex == GDMSex.svMale) ? (int)fStats.life.MaleVal : (int)fStats.life.FemaleVal;
            
            if ((yBirth != 0) && (yDeath == 0)) {
                yDeath = yBirth + provedLife; //GKData.PROVED_LIFE_LENGTH;
            }

            if ((yBirth == 0) && (yDeath != 0)) {
                yBirth = yDeath - provedLife; //GKData.PROVED_LIFE_LENGTH;
            }

            var range = new Range<int>(yBirth, yDeath);
            return range;
        }

        private static string GetPersonalInfo(GDMIndividualRecord iRec)
        {
            return GKUtils.GetNameString(iRec, true, false) + GKUtils.GetLifeStr(iRec);
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
            IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

            fTitleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            fChapFont = fWriter.CreateFont("", 16f, true, false, clrBlack);
            fTextFont = fWriter.CreateFont("", 10f, false, false, clrBlack);

            var stats = new TreeStats(fBase.Context, fBase.GetContentList(GDMRecordType.rtIndividual));
            fStats = stats.GetCommonStats();

            fWriter.AddParagraph(fTitle, fTitleFont, TextAlignment.taLeft);

            var personRange = GetIndividualDates(fPerson);
            fWriter.AddParagraph(GetPersonalInfo(fPerson), fChapFont, TextAlignment.taLeft);
            
            fWriter.BeginList();

            var enumer = fBase.Context.Tree.GetEnumerator(GDMRecordType.rtIndividual);
            GDMRecord record;
            while (enumer.MoveNext(out record)) {
                var iRec = record as GDMIndividualRecord;
                var indRange = GetIndividualDates(iRec);
                try {
                    if (personRange.IsOverlapped(indRange)) {
                        fWriter.AddListItem(" " + GetPersonalInfo(iRec), fTextFont);
                    }
                } catch (Exception ex) {
                    Logger.LogWrite("ContemporariesReport.InternalGenerate(): " + ex.Message);
                }
            }

            fWriter.EndList();
        }
    }
}
