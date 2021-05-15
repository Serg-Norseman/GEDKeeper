﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

using GDModel;
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using GKUI.Platform;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class RecordInfoDlgTests : CustomWindowTest
    {
        private GDMIndividualRecord fIndividual;
        private IBaseWindow fBase;
        private RecordInfoDlg fDialog;

        public override void Setup()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();
            fIndividual = new GDMIndividualRecord(null);

            fDialog = new RecordInfoDlg(fBase);
            fDialog.Record = fIndividual;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fIndividual.Dispose();
        }

        [Test]
        public void Test_Common()
        {
            Assert.AreEqual(fIndividual, fDialog.Record);
        }
    }
}

#endif
