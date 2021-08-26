/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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

using System.Windows.Forms;
using NUnit.Framework;
using GKTests;

namespace GKUI.Components
{
    [TestFixture]
    public class SmallComponents : CustomWindowTest
    {
        public SmallComponents()
        {
        }

        [Test]
        public void Test_GKTabControl()
        {
            using (var form = new Form()) {
                form.ClientSize = new System.Drawing.Size(383, 221);

                var comp = new GKTabControl();
                comp.Dock = DockStyle.Fill;

                var tabPage = new TabPage();
                comp.Controls.Add(tabPage);

                form.SuspendLayout();
                form.Controls.Add(comp);
                form.ResumeLayout(false);
                form.PerformLayout();

                form.Show();
                form.Close();
            }
        }

        [Test]
        public void Test_GKTextBox()
        {
            using (var form = new Form()) {
                form.ClientSize = new System.Drawing.Size(383, 221);

                var comp = new GKTextBox();
                comp.Name = "testTextBox";
                comp.Dock = DockStyle.Fill;

                comp.NameMode = true;
                Assert.AreEqual(true, comp.NameMode);
                comp.Text = "sample";
                Assert.AreEqual("sample", comp.Text);

                comp.Text = "";
                Assert.AreEqual("", comp.Text);
                comp.Numeric = true;
                Assert.AreEqual(true, comp.Numeric);
                comp.Text = "";
                Assert.AreEqual("0", comp.Text);

                comp.Trimmed = true;
                Assert.AreEqual(true, comp.Trimmed);

                form.SuspendLayout();
                form.Controls.Add(comp);
                form.ResumeLayout(false);
                form.PerformLayout();

                form.Show();

                /*var ctl = new TextBoxTester("testTextBox", form);
                Keyboard.UseOn(ctl);
                Keyboard.Press("7");
                Assert.AreEqual("7", comp.Text);
                Keyboard.Press("A");
                Assert.AreEqual("7", comp.Text);*/

                form.Close();
            }
        }

        [Test]
        public void Test_WizardPages()
        {
            using (var form = new Form()) {
                form.ClientSize = new System.Drawing.Size(383, 221);

                var comp = new WizardPages();
                comp.Dock = DockStyle.Fill;

                form.SuspendLayout();
                form.Controls.Add(comp);
                form.ResumeLayout(false);
                form.PerformLayout();

                form.Show();
                form.Close();
            }
        }

        [Test]
        public void Test_GKComboBox()
        {
            using (var form = new Form()) {
                form.ClientSize = new System.Drawing.Size(383, 221);

                var comp = new GKComboBox();
                comp.Dock = DockStyle.Fill;

                form.SuspendLayout();
                form.Controls.Add(comp);
                form.ResumeLayout(false);
                form.PerformLayout();

                form.Show();
                form.Close();
            }
        }

        [Test]
        public void Test_GKMapBrowser()
        {
            using (var form = new Form()) {
                form.ClientSize = new System.Drawing.Size(383, 221);

                var comp = new GKMapBrowser();
                comp.Dock = DockStyle.Fill;

                form.SuspendLayout();
                form.Controls.Add(comp);
                form.ResumeLayout(false);
                form.PerformLayout();

                comp.ShowPoints = true;
                Assert.AreEqual(true, comp.ShowPoints);

                comp.ShowLines = true;
                Assert.AreEqual(true, comp.ShowLines);

                Assert.IsNotNull(comp.MapPoints);

                comp.BeginUpdate();
                comp.SetCenter(0, 0, 1);
                comp.EndUpdate(); // -> RefreshPoints()

                form.Show();
                form.Close();
            }
        }

        [Test]
        public void Test_ArborViewer()
        {
            using (var form = new Form()) {
                form.ClientSize = new System.Drawing.Size(383, 221);

                var comp = new ArborViewer();
                comp.Dock = DockStyle.Fill;

                comp.Sys.AddNode("A");
                comp.Sys.AddNode("B");
                comp.Sys.AddNode("C");
                comp.Sys.AddEdge("A", "B");
                comp.Sys.AddEdge("B", "C");
                comp.start();

                comp.EnergyDebug = true;
                Assert.AreEqual(true, comp.EnergyDebug);

                comp.NodesDragging = true;
                Assert.AreEqual(true, comp.NodesDragging);

                form.SuspendLayout();
                form.Controls.Add(comp);
                form.ResumeLayout(true);

                form.Show();
                form.Invalidate();
                form.Close();
            }
        }
    }
}

#endif
