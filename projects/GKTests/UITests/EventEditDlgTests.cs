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

using System;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKTests.Mocks;
using GKUI.Dialogs;
using NUnit.Framework;

namespace GKTests.UITests
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class EventEditDlgTests : CustomWindowTest
    {
        private GEDCOMCustomEvent fEvent;
        private IBaseWindow fBase;
        private EventEditDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowMock();
            fEvent = new GEDCOMIndividualEvent(fBase.Context.Tree, null, "", "");

            fDialog = new EventEditDlg(fBase);
            fDialog.Event = fEvent;
        }

        [STAThread]
        [Test]
        public void Test_Date1()
        {
            fEvent.Date.ParseString("3 MAY 1835");
            fDialog.Event = fEvent;
            fDialog.Show();
            ClickButton("btnAccept", fDialog);
        }

        [STAThread]
        [Test]
        public void Test_Date2()
        {
            fEvent.Date.ParseString("ABT 1844");
            fDialog.Event = fEvent;
            fDialog.Show();
            ClickButton("btnAccept", fDialog);
        }

        //

        [STAThread]
        [Test]
        public void Test_Date3()
        {
            fEvent.Date.ParseString("ABT 20 JAN 2013");
            fDialog.Event = fEvent;
            fDialog.Show();
            ClickButton("btnAccept", fDialog);
        }

        [STAThread]
        [Test]
        public void Test_Date4()
        {
            fEvent.Date.ParseString("CAL 20 JAN 2013");
            fDialog.Event = fEvent;
            fDialog.Show();
            ClickButton("btnAccept", fDialog);
        }

        [STAThread]
        [Test]
        public void Test_Date5()
        {
            fEvent.Date.ParseString("EST 20 DEC 2013");
            fDialog.Event = fEvent;
            fDialog.Show();
            ClickButton("btnAccept", fDialog);
        }

        [STAThread]
        [Test]
        public void Test_Date6()
        {
            fEvent.Date.ParseString("FROM 04 JAN 2013 TO 23 JAN 2013");
            fDialog.Event = fEvent;
            fDialog.Show();
            ClickButton("btnAccept", fDialog);
        }

        [STAThread]
        [Test]
        public void Test_Date7()
        {
            fEvent.Date.ParseString("BEF 20 JAN 2013");
            fDialog.Event = fEvent;
            fDialog.Show();
            ClickButton("btnAccept", fDialog);
        }

        [STAThread]
        [Test]
        public void Test_Date8()
        {
            fEvent.Date.ParseString("AFT 20 JAN 2013");
            fDialog.Event = fEvent;
            fDialog.Show();
            ClickButton("btnAccept", fDialog);
        }

        [STAThread]
        [Test]
        public void Test_Date9()
        {
            fEvent.Date.ParseString("BET 04 JAN 2013 AND 25 JAN 2013");
            fDialog.Event = fEvent;
            fDialog.Show();
            ClickButton("btnAccept", fDialog);
        }

        [STAThread]
        [Test]
        public void Test_Date10()
        {
            fEvent.Date.ParseString("FROM 04 JAN 2013");
            fDialog.Event = fEvent;
            fDialog.Show();
            ClickButton("btnAccept", fDialog);
        }

        [STAThread]
        [Test]
        public void Test_Date11()
        {
            fEvent.Date.ParseString("TO 23 JAN 2013");
            fDialog.Event = fEvent;
            fDialog.Show();
            ClickButton("btnAccept", fDialog);
        }

        [STAThread]
        [Test]
        public void Test_Cancel()
        {
            fDialog.Show();

            Assert.AreEqual(fBase, fDialog.Base);
            Assert.AreEqual(fEvent, fDialog.Event);

            // The links to other records can be added or edited only in MainWinTests
            // (where there is a complete infrastructure of the calls to BaseWin.ModifyX)

            ClickButton("btnCancel", fDialog);
        }
    }
}

#endif
