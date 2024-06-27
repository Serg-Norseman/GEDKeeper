/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

#if !MONO && !DIS_NUF

using System;
using System.Threading;
using System.Windows.Forms;
using GDModel;
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using NUnit.Framework;

namespace GKUI.Forms
{
    [TestFixture]
    public class EventEditDlgTests : CustomWindowTest
    {
        private GDMCustomEvent fEvent;
        private IBaseWindow fBase;
        private EventEditDlg fDialog;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();

            fBase = new BaseWindowStub();
            fEvent = new GDMIndividualEvent();

            fDialog = new EventEditDlg(fBase);
            fDialog.Event = fEvent;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fEvent.Dispose();
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Date1()
        {
            fEvent.Date.ParseString("3 MAY 1835");
            fDialog.Event = fEvent;
            ClickButton("btnAccept", fDialog);
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Date2()
        {
            fEvent.Date.ParseString("ABT 1844");
            fDialog.Event = fEvent;
            ClickButton("btnAccept", fDialog);
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Date3()
        {
            fEvent.Date.ParseString("ABT 20 JAN 2013");
            fDialog.Event = fEvent;
            ClickButton("btnAccept", fDialog);
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Date4()
        {
            fEvent.Date.ParseString("CAL 20 JAN 2013");
            fDialog.Event = fEvent;
            ClickButton("btnAccept", fDialog);
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Date5()
        {
            fEvent.Date.ParseString("EST 20 DEC 2013");
            fDialog.Event = fEvent;
            ClickButton("btnAccept", fDialog);
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Date6()
        {
            fEvent.Date.ParseString("FROM 04 JAN 2013 TO 23 JAN 2013");
            fDialog.Event = fEvent;
            ClickButton("btnAccept", fDialog);
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Date7()
        {
            fEvent.Date.ParseString("BEF 20 JAN 2013");
            fDialog.Event = fEvent;
            ClickButton("btnAccept", fDialog);
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Date8()
        {
            fEvent.Date.ParseString("AFT 20 JAN 2013");
            fDialog.Event = fEvent;
            ClickButton("btnAccept", fDialog);
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Date9()
        {
            fEvent.Date.ParseString("BET 04 JAN 2013 AND 25 JAN 2013");
            fDialog.Event = fEvent;
            ClickButton("btnAccept", fDialog);
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Date10()
        {
            fEvent.Date.ParseString("FROM 04 JAN 2013");
            fDialog.Event = fEvent;
            ClickButton("btnAccept", fDialog);
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Date11()
        {
            fEvent.Date.ParseString("TO 23 JAN 2013");
            fDialog.Event = fEvent;
            ClickButton("btnAccept", fDialog);
        }

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_Cancel()
        {
            Assert.AreEqual(fEvent, fDialog.Event);

            // The links to other records can be added or edited only in MainWinTests
            // (where there is a complete infrastructure of the calls to BaseWin.ModifyX)

            ClickButton("btnCancel", fDialog);
        }

        #region Handlers for external tests

        public static void EventEditDlg_Select_Handler(string name, IntPtr ptr, Form form)
        {
            EventEditDlg eventDlg = (EventEditDlg) form;
            Assert.IsNotNull(eventDlg.Event);

            SelectCombo("cmbEventType", form, 1); // Birth(indi) / ?(fam)
            EnterText("txtEventPlace", form, "test place");

            // FIXME: create GKDateControl tests
            /*SelectCombo("cmbEventDateType", form, 3); // Between
            EnterMaskedText("txtEventDate1", form, "01.01.1900");
            EnterMaskedText("txtEventDate2", form, "10.01.1900");
            SelectCombo("cmbDate1Calendar", form, 1); // Julian
            SelectCombo("cmbDate2Calendar", form, 1); // Julian*/

            EnterCombo("txtEventCause", form, "test cause");
            EnterCombo("txtEventOrg", form, "test agency");

            SetModalFormHandler(fFormTest, AddressEditDlgTests.AddressEditDlg_btnAccept_Handler);
            ClickButton("btnAddress", form);

            RecordSelectDlgTests.SetCreateItemHandler(fFormTest, LocationEditDlgTests.LocationAdd_Mini_Handler);
            ClickButton("btnPlaceAdd", form);

            ClickButton("btnPlaceDelete", form);

            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
