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
using System.Drawing;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI;
using GKUI.Charts;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
    public sealed class TreesAlbumExporter : PDFExporter
    {
        private ShieldState fShieldState;
        private StringList fPatList;

        private iTextSharp.text.Font fTitleFont;
        //private Font fChapFont;
        private iTextSharp.text.Font fPersonFont;
        //private Font fLinkFont;
        private iTextSharp.text.Font fTextFont;
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
                    fTitleFont = new iTextSharp.text.Font(baseFont, 20f, iTextSharp.text.Font.BOLD);
                    //fChapFont = new Font(baseFont, 16f, Font.BOLD, BaseColor.BLACK);
                    fPersonFont = new iTextSharp.text.Font(baseFont, 10f, iTextSharp.text.Font.BOLD, BaseColor.BLACK);
                    //fLinkFont = new Font(baseFont, 8f, Font.UNDERLINE, BaseColor.BLUE);
                    fTextFont = new iTextSharp.text.Font(baseFont, 8f, iTextSharp.text.Font.NORMAL, BaseColor.BLACK);
                    //fSupText = new Font(baseFont, 5f, Font.NORMAL, BaseColor.BLUE);

                    fDocument.Add(new Paragraph(title, fTitleFont) { Alignment = Element.ALIGN_CENTER, SpacingAfter = 6f });

                    PreparePatriarchs();

                    iTextSharp.text.Rectangle pageSize = fDocument.PageSize;
                    ExtRect pageRect = ExtRect.CreateBounds(0, 0, (int)pageSize.Width, (int)pageSize.Height);
                    ExtRect clientRect = ExtRect.CreateBounds(fMargins.Left, fMargins.Top,
                                                              (int)pageSize.Width - fMargins.Left - fMargins.Right,
                                                              (int)pageSize.Height - fMargins.Top - fMargins.Bottom);

                    var renderer = new TreeChartPDFRenderer(true);
                    renderer.SetTarget(fPdfWriter.DirectContent);
                    PdfContentByte cb = fPdfWriter.DirectContent;

                    var treeBox = new TreeChartBox(renderer);
                    treeBox.Base = fBase;
                    treeBox.Options = MainWin.Instance.Options.ChartOptions;
                    treeBox.DepthLimit = 2;
                    treeBox.ShieldState = fBase.ShieldState;
                    treeBox.Height = clientRect.GetHeight();
                    treeBox.Width = clientRect.GetWidth();

                    int num = fPatList.Count;
                    for (int i = 0; i < num; i++) {
                        string iName = fPatList[i];
                        GEDCOMIndividualRecord iRec = fPatList.GetObject(i) as GEDCOMIndividualRecord;
                        fDocument.Add(new Paragraph(iName, fTextFont) { Alignment = Element.ALIGN_LEFT, SpacingBefore = 2f, SpacingAfter = 2f });

                        cb.SetFontAndSize(baseFont, 6);

                        treeBox.GenChart(iRec, TreeChartBox.ChartKind.ckDescendants, true);

                        renderer.SetSizes(new Size(pageRect.GetWidth(), pageRect.GetHeight()), treeBox.ImageSize);

                        treeBox.RenderStatic();

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
