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

#if !__MonoCS__

using System;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKTests.Mocks;
using GKUI.Dialogs;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKTests.UITests
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class EventEditDlgTests : NUnitFormTest
    {
        public EventEditDlgTests()
        {
        }

        private IBaseContext fContext;
        private GEDCOMCustomEvent fEvent;
        private IBaseWindow fBase;

        private EventEditDlg _frm;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowMock();
            fContext = fBase.Context;
            fEvent = new GEDCOMIndividualEvent(fContext.Tree, fContext.Tree, "", "");

            //ExpectModal("EventEditDlg", "DlgHandler");
            _frm = new EventEditDlg(fBase);
            _frm.Event = fEvent;
            //_frm.ShowDialog();
            //_frm.Show();
        }

        [Test]
        public void Test_Misc()
        {
            Assert.AreEqual(fBase, _frm.Base);
            Assert.AreEqual(fEvent, _frm.Event);
        }

        [Test]
        public void Test_btnCancel()
        {
            //var btnCancel = new ButtonTester("btnCancel");
            //btnCancel.Click();
        }
    }
}

#endif
