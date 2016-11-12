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
    public class DayTipsDlgTests : NUnitFormTest
    {
        public DayTipsDlgTests()
        {
        }

        private IBaseContext fContext;
        private IBaseWindow fBase;

        private DayTipsDlg _frm;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowMock();
            fContext = fBase.Context;

            //ExpectModal("DayTipsDlg", "DlgHandler");
            _frm = new DayTipsDlg();
            //_frm.ShowDialog();
            _frm.Show();
        }

        [Test]
        public void Test_Misc()
        {
        }

        [Test]
        public void Test_btnCancel()
        {
            var btnClose = new ButtonTester("btnClose");
            btnClose.Click();
        }

        [Test]
        public void Test_EnterTextAndAccept()
        {
            /*var txtShortTitle = new TextBoxTester("txtShortTitle");
            txtShortTitle.Enter("sample text");
            Assert.AreEqual("sample text", txtShortTitle.Text);

            var txtAuthor = new TextBoxTester("txtAuthor");
            txtAuthor.Enter("sample text");
            Assert.AreEqual("sample text", txtAuthor.Text);*/

            //var btnAccept = new ButtonTester("btnAccept");
            //btnAccept.Click();

            //Assert.AreEqual("sample text", fTaskRecord.FiledByEntry);
            //Assert.AreEqual("sample text\r\n", fTaskRecord.Originator.Text);
        }
    }
}

#endif
