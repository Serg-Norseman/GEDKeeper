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

#if !MONO

using GKCore.Interfaces;
using GKTests;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class ProgressDlgTests : CustomWindowTest
    {
        private ProgressDlg fDialog;

        public override void Setup()
        {
            fDialog = new ProgressDlg();
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Common()
        {
            fDialog.Begin(100, true);
            fDialog.SetText("Test1");
            Assert.IsFalse(fDialog.IsCanceled);
            fDialog.Increment();
            fDialog.StepTo(10);
            fDialog.End();
        }

        [Test]
        public void Test_Common2()
        {
            fDialog.Begin("Test2", 100, true);
            fDialog.InvokeEx(() => { });
            fDialog.End(new ThreadError(10, "fail"));
            fDialog.Close();
        }
    }
}

#endif
