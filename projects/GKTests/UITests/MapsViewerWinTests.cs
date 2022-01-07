/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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

#if !MONO

using System.Windows.Forms;
using GKTests;
using GKUI.Platform;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class MapsViewerWinTests : CustomWindowTest
    {
        public override void Setup()
        {
            WFAppHost.ConfigureBootstrap(false);
        }

        public override void TearDown()
        {
        }

        [Test]
        public void Test_Common()
        {
        }

        #region Handlers for external tests

        public static void MapsViewerWin_Handler(CustomWindowTest formTest, Form form, string stageMessage)
        {
            Assert.IsInstanceOf(typeof(MapsViewerWin), form, stageMessage);

            ClickRadioButton("radTotal", form);

            formTest.ModalFormHandler = SaveFile_Cancel_Handler;
            ClickToolStripButton("tbSaveSnapshot", form);

            KeyDownForm(form.Name, Keys.Escape);
            form.Dispose();
        }

        #endregion
    }
}

#endif
