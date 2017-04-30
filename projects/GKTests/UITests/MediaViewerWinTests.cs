/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

#if !__MonoCS__

using System.Drawing;
using System.Windows.Forms;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKTests.GEDCOM;
using GKTests.Mocks;
using GKUI;
using NUnit.Framework;

namespace GKTests.UITests
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class MediaViewerWinTests : CustomWindowTest
    {
        private IBaseWindow fBase;
        private MediaViewerWin fDialog;
        private GEDCOMFileReferenceWithTitle fileRef;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowMock();

            GEDCOMMultimediaRecord mmRec = fBase.Context.Tree.CreateMultimedia();
            mmRec.AddTag("FILE", "", null);
            fileRef = mmRec.FileReferences[0];
            fileRef.Title = "File Title 2";
            fileRef.LinkFile("shaytan_plant.jpg");
            fileRef.MediaType = GEDCOMMediaType.mtPhoto;

            fDialog = new MediaViewerWin(fBase);
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Common()
        {
            Assert.AreEqual(null, fDialog.FileRef);
            fDialog.FileRef = fileRef;
            Assert.AreEqual(fileRef, fDialog.FileRef);

            GKResourceManager resMgr = new GKResourceManager("GKTests.GXResources", typeof(GedcomTests).Assembly);
            Bitmap img = (Bitmap)resMgr.GetObjectEx("shaytan_plant");
            fDialog.SetViewImage(img);

            fDialog.Refresh();

            ClickToolStripButton("btnZoomIn", fDialog);
            ClickToolStripButton("btnZoomOut", fDialog);
            ClickToolStripButton("btnSizeToFit", fDialog);

            KeyDownForm(fDialog.Name, Keys.Escape);
        }
    }
}

#endif
