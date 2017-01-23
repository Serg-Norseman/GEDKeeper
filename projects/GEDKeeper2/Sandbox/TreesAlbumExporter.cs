/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
    public sealed class TreesAlbumExporter : PDFExporter
    {
        private ShieldState fShieldState;
        private StringList fPatList;

        private Font fTitleFont;
        //private Font fChapFont;
        private Font fPersonFont;
        //private Font fLinkFont;
        private Font fTextFont;
        //private Font fSupText;

        public ShieldState ShieldState
        {
            get { return fShieldState; }
            set { fShieldState = value; }
        }

        public TreesAlbumExporter(IBaseWindow baseWin) : base(baseWin)
        {
            fPatList = new StringList();
        }

        protected override void InternalGenerate()
        {
            try
            {
                {
                    string title = "Trees Album";//LangMan.LS(LSID.LSID_ExpPedigree) + ": " + GKUtils.GetNameString(fAncestor, true, false);

                    fDocument.AddTitle(title);
                    fDocument.AddSubject("");
                    fDocument.AddAuthor("");
                    fDocument.AddCreator(GKData.APP_TITLE);
                    fDocument.Open();

                    BaseFont baseFont = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Times.ttf"), BaseFont.IDENTITY_H, BaseFont.EMBEDDED);
                    fTitleFont = new Font(baseFont, 20f, Font.BOLD);
                    //fChapFont = new Font(baseFont, 16f, Font.BOLD, BaseColor.BLACK);
                    fPersonFont = new Font(baseFont, 10f, Font.BOLD, BaseColor.BLACK);
                    //fLinkFont = new Font(baseFont, 8f, Font.UNDERLINE, BaseColor.BLUE);
                    fTextFont = new Font(baseFont, 8f, Font.NORMAL, BaseColor.BLACK);
                    //fSupText = new Font(baseFont, 5f, Font.NORMAL, BaseColor.BLUE);

                    fDocument.Add(new Paragraph(title, fTitleFont) { Alignment = Element.ALIGN_CENTER, SpacingAfter = 6f });

                    PreparePatriarchs();

                    Rectangle pageSize = fDocument.PageSize;
                    ExtRect clientRect = ExtRect.CreateBounds(fMargins.Left, fMargins.Top,
                                                              (int)pageSize.Width - fMargins.Left - fMargins.Right,
                                                              (int)pageSize.Height - fMargins.Top - fMargins.Bottom);

                    float cx = (pageSize.Width - fMargins.Left - fMargins.Right) / 2;
                    float cy = (pageSize.Height - fMargins.Top - fMargins.Bottom) / 2;

                    TreeChartPDFRenderer renderer = new TreeChartPDFRenderer();
                    renderer.SetTarget(fPdfWriter.DirectContent);
                    PdfContentByte cb = fPdfWriter.DirectContent;

                    int num = fPatList.Count;
                    for (int i = 0; i < num; i++) {
                        string iName = fPatList[i];
                        GEDCOMIndividualRecord iRec = fPatList.GetObject(i) as GEDCOMIndividualRecord;
                        fDocument.Add(new Paragraph(iName, fTextFont) { Alignment = Element.ALIGN_LEFT, SpacingBefore = 2f, SpacingAfter = 2f });

                        renderer.DrawLine(null, clientRect.Left, clientRect.Top + 200, clientRect.Right, clientRect.Top + 200);

                        cb.BeginText();
                        cb.SetFontAndSize(baseFont, 6);
                        cb.SetTextMatrix(cx, cy);
                        cb.ShowText("Some text here and the Date: " + DateTime.Now.ToShortDateString());
                        cb.EndText();

                        fDocument.NewPage();
                    }
                }
            }
            catch (Exception)
            {
                throw;
            }
        }

        private void PreparePatriarchs()
        {
            using (ExtList<PatriarchObj> lst = fBase.Context.GetPatriarchsList(2, false))
            {
                int num = lst.Count;
                for (int i = 0; i < num; i++) {
                    PatriarchObj pObj = lst[i];
                    fPatList.AddObject(GKUtils.GetNameString(pObj.IRec, true, false), pObj.IRec);
                }
                fPatList.Sort();
            }
        }
    }
}
