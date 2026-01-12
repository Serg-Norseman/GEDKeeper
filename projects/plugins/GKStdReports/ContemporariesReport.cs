/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Export;
using GKCore.Export.Formats;
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
        private CommonStats fStats;

        public ContemporariesReport(IBaseWindow baseWin, GDMIndividualRecord selectedPerson)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(PLS.ContemporariesReport);
            fPerson = selectedPerson;
        }

        private Range<int> GetIndividualDates(GDMIndividualRecord iRec)
        {
            var dates = iRec.GetLifeEvents();

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

            var titleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            var chapFont = fWriter.CreateFont("", 16f, true, false, clrBlack);
            var textFont = fWriter.CreateFont("", 10f, false, false, clrBlack);

            var stats = new TreeStats(fBase.Context, fBase.GetContentList(GDMRecordType.rtIndividual));
            fStats = stats.GetCommonStats();

            fWriter.AddParagraph(fTitle, titleFont, TextAlignment.taLeft);

            var personRange = GetIndividualDates(fPerson);
            fWriter.AddParagraph(GetPersonalInfo(fPerson), chapFont, TextAlignment.taLeft);
            
            fWriter.BeginList();

            var enumer = fBase.Context.Tree.GetEnumerator<GDMIndividualRecord>();
            GDMIndividualRecord iRec;
            while (enumer.MoveNext(out iRec)) {
                var indRange = GetIndividualDates(iRec);
                try {
                    if (personRange.IsOverlapped(indRange)) {
                        fWriter.AddListItem(" " + GetPersonalInfo(iRec), textFont);
                    }
                } catch (Exception ex) {
                    Logger.WriteError("ContemporariesReport.InternalGenerate()", ex);
                }
            }

            fWriter.EndList();
        }
    }
}
