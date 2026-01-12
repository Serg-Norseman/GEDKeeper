/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Export;
using GKCore.Export.Formats;
using GKCore.Linguistics;

namespace GKStdReports
{
    public sealed class PhoneticsReport : ReportExporter
    {
        public PhoneticsReport(IBaseWindow baseWin)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(PLS.PhoneticsReport);
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);

            var titleFont = fWriter.CreateFont("", 22f, true, false, clrBlack);
            var chapFont = fWriter.CreateFont("", 16f, true, false, clrBlack);
            var textFont = fWriter.CreateFont("", 10f, false, false, clrBlack);

            fWriter.AddParagraph(fTitle, titleFont, TextAlignment.taLeft);

            var surnames = new StringList();
            surnames.Sorted = true;
            surnames.DuplicateSolve = DuplicateSolve.Ignore;

            GDMTree tree = fBase.Context.Tree;
            var enumer = tree.GetEnumerator<GDMIndividualRecord>();
            GDMIndividualRecord iRec;
            while (enumer.MoveNext(out iRec)) {
                var nameParts = GKUtils.GetNameParts(tree, iRec, false);
                string surname = fBase.Context.Culture.NormalizeSurname(nameParts.Surname, iRec.Sex == GDMSex.svFemale);
                surnames.Add(surname);
            }

            fWriter.AddParagraph(SRLangMan.LS(PLS.Surnames), chapFont, TextAlignment.taLeft);
            fWriter.BeginList();
            for (int i = 0; i < surnames.Count; i++) {
                string item = surnames[i];
                string primaryKey = "", alternateKey = "";
                string translit = Morpher.Transliterate(TranslitScheme.ts_Russian, TranslitScheme.ts_GOST, item);
                DoubleMetaphone.doubleMetaphone(translit, ref primaryKey, ref alternateKey);
                fWriter.AddListItem(" " + item + "\t" + translit + "\t" + primaryKey + "\t" + alternateKey, textFont);
            }
            fWriter.EndList();
        }
    }
}
