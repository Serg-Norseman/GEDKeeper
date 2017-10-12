using System;
using System.Collections.Generic;
using Eto;
using Eto.Forms;
using GKCommon;
using NUnit.Framework;

namespace GKUI.Forms
{
    [TestFixture]
    public class UITests
    {
        [STAThread]
        [TestFixtureSetUp]
        public void SetUp()
        {
            //EtoFormsAppHost.ConfigureBootstrap(false);
            //Logger.LogInit(EtoFormsAppHost.GetLogFilename());
            //new Application(Platforms.Wpf);
        }

        [TestFixtureTearDown]
        public void TearDown()
        {
        }

        public UITests()
        {
        }

        [STAThread]
        [Test]
        public void AboutDlg_Tests()
        {
            //var dlg = new AboutDlg();
        }
    }
}