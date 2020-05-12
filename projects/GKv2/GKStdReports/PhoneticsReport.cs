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
using BSLib.Linguistics.Grammar;
using GDModel;
using GKCore;
using GKCore.Export;
using GKCore.Interfaces;

namespace GKStdReports
{
    public sealed class PhoneticsReport : ReportExporter
    {
        private IFont fTitleFont, fChapFont, fTextFont;

        public PhoneticsReport(IBaseWindow baseWin)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(RLS.LSID_Phonetics_Title);
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
            IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

            fTitleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            fChapFont = fWriter.CreateFont("", 16f, true, false, clrBlack);
            fTextFont = fWriter.CreateFont("", 10f, false, false, clrBlack);

            fWriter.AddParagraph(fTitle, fTitleFont, TextAlignment.taLeft);

            var surnames = new StringList();
            surnames.Sorted = true;
            surnames.DuplicateSolve = DuplicateSolve.Ignore;

            GDMTree tree = fBase.Context.Tree;
            var enumer = tree.GetEnumerator(GDMRecordType.rtIndividual);
            GDMRecord record;
            while (enumer.MoveNext(out record)) {
                var iRec = record as GDMIndividualRecord;
                var nameParts = GKUtils.GetNameParts(iRec, false);
                string surname = fBase.Context.Culture.NormalizeSurname(nameParts.Surname, iRec.Sex == GDMSex.svFemale);
                surnames.Add(surname);
            }

            fWriter.AddParagraph(SRLangMan.LS(RLS.LSID_Surnames), fChapFont, TextAlignment.taLeft);
            fWriter.BeginList();
            for (int i = 0; i < surnames.Count; i++) {
                string item = surnames[i];
                string primaryKey = "", alternateKey = "";
                string translit = BaseMorpher.Transliterate(TranslitScheme.ts_Russian, TranslitScheme.ts_GOST, item);
                DoubleMetaphone.doubleMetaphone(translit, ref primaryKey, ref alternateKey);
                fWriter.AddListItem(" " + item + "\t" + translit + "\t" + primaryKey + "\t" + alternateKey, fTextFont);
            }
            fWriter.EndList();
        }
    }
}
