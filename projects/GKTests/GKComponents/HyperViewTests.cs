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

using System.Drawing;
using System.Windows.Forms;
using NUnit.Framework;

namespace GKUI.Components
{
    [TestFixture]
    public class HyperViewTests
    {
        private Form fForm;
        private HyperView fHyperView;

        [TestFixtureSetUp]
        public void Init()
        {
            fForm = new Form();
            fForm.ClientSize = new Size(383, 221);
            fForm.Text = "ImageViewTests";

            fHyperView = new HyperView();
            fHyperView.Dock = DockStyle.Fill;

            fForm.SuspendLayout();
            fForm.Controls.Add(fHyperView);
            fForm.ResumeLayout(false);
            fForm.PerformLayout();
        }

        [TestFixtureTearDown]
        public void Done()
        {
            fForm.Dispose();
        }

        [Test]
        public void TestMethod()
        {
            fHyperView.BorderWidth = 3;
            Assert.AreEqual(3, fHyperView.BorderWidth);

            fHyperView.LinkColor = Color.Red;
            Assert.AreEqual(Color.Red, fHyperView.LinkColor);

            fForm.Show();

            fForm.Close();
        }
    }
}

#endif
