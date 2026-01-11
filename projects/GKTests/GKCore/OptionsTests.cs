// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System;
using BSLib;
using GDModel;
using GKCore.Backups;
using GKCore.Names;
using GKTests;
using NUnit.Framework;

namespace GKCore.Options
{
    [TestFixture]
    public class OptionsTests
    {
        private readonly BaseContext fContext;

        public OptionsTests()
        {
            TestUtils.InitUITest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Locales()
        {
            LangRecord langRecord = new LangRecord(1049, "rus", "Russian", "filename.lng", null);
            Assert.IsNotNull(langRecord);
            Assert.AreEqual(1049, langRecord.Code);
            Assert.AreEqual("rus", langRecord.Sign);
            Assert.AreEqual("Russian", langRecord.Name);
            Assert.AreEqual("filename.lng", langRecord.FileName);
        }

        [Test]
        public void Test_Options()
        {
            using (IniFile iniFile = new IniFile()) {
                GlobalOptions globalOptions = GlobalOptions.Instance;
                Assert.IsNotNull(globalOptions);

                Assert.IsNotNull(globalOptions.TreeChartOptions);
                Assert.IsNotNull(globalOptions.CircleChartOptions);

                /*globalOptions.DefCharacterSet = GEDCOMCharacterSet.csUNICODE;
                Assert.AreEqual(GEDCOMCharacterSet.csUNICODE, globalOptions.DefCharacterSet);*/
                Assert.AreEqual(GEDCOMCharacterSet.csUTF8, globalOptions.DefCharacterSet);

                globalOptions.DefDateFormat = DateFormat.dfDD_MM_YYYY;
                Assert.AreEqual(DateFormat.dfDD_MM_YYYY, globalOptions.DefDateFormat);

                globalOptions.ShowDatesSign = true;
                Assert.AreEqual(true, globalOptions.ShowDatesSign);

                globalOptions.DefNameFormat = NameFormat.nfF_N_P;
                Assert.AreEqual(NameFormat.nfF_N_P, globalOptions.DefNameFormat);

                Assert.IsNotNull(globalOptions.EventFilters);

                globalOptions.InterfaceLang = 1000;
                Assert.AreEqual(1000, globalOptions.InterfaceLang);

                globalOptions.LastDir = "c:\\";
                Assert.AreEqual("c:\\", globalOptions.LastDir);

                Assert.IsNotNull(globalOptions.MRUFiles);

                globalOptions.MWinRect = ExtRect.CreateEmpty();
                Assert.IsTrue(globalOptions.MWinRect.IsEmpty());

                globalOptions.MWinState = WindowState.Maximized;
                Assert.AreEqual(WindowState.Maximized, globalOptions.MWinState);

                Assert.IsNotNull(globalOptions.NameFilters);

                Assert.IsNotNull(globalOptions.PedigreeOptions);

                globalOptions.PlacesWithAddress = true;
                Assert.AreEqual(true, globalOptions.PlacesWithAddress);

                Assert.IsNotNull(globalOptions.Proxy);

                Assert.IsNotNull(globalOptions.Relations);

                Assert.IsNotNull(globalOptions.ResidenceFilters);

                globalOptions.Backups.FileBackup = FileBackup.fbOnlyPrev;
                Assert.AreEqual(FileBackup.fbOnlyPrev, globalOptions.Backups.FileBackup);

                globalOptions.ShowTips = true;
                Assert.AreEqual(true, globalOptions.ShowTips);

                globalOptions.ListHighlightUnmarriedPersons = true;
                Assert.AreEqual(true, globalOptions.ListHighlightUnmarriedPersons);

                globalOptions.ListHighlightUnparentedPersons = true;
                Assert.AreEqual(true, globalOptions.ListHighlightUnparentedPersons);

                Assert.IsNotNull(globalOptions.ListOptions[GDMRecordType.rtIndividual].Columns);

                globalOptions.ShowDatesCalendar = true;
                Assert.AreEqual(true, globalOptions.ShowDatesCalendar);

                globalOptions.Backups.Autosave = true;
                Assert.AreEqual(true, globalOptions.Backups.Autosave);

                globalOptions.Backups.AutosaveInterval = 10;
                Assert.AreEqual(10, globalOptions.Backups.AutosaveInterval);

                globalOptions.UseExtendedNotes = true;
                Assert.AreEqual(true, globalOptions.UseExtendedNotes);
                globalOptions.UseExtendedNotes = false;

                globalOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden;
                Assert.AreEqual(WomanSurnameFormat.wsfMaiden, globalOptions.WomanSurnameFormat);

                globalOptions.LastBases.Add("sample.ged");
                Assert.AreEqual(1, globalOptions.LastBases.Count);
                Assert.AreEqual("sample.ged", globalOptions.LastBases[0]);
                globalOptions.LastBases.Clear();

                Assert.IsNotNull(globalOptions.Languages);

                globalOptions.FindLanguages();


                globalOptions.SaveToFile(iniFile);
                globalOptions.LoadFromFile(iniFile);

                IniFile ini = null;
                Assert.Throws(typeof(ArgumentNullException), () => { globalOptions.SaveToFile(ini); });
                Assert.Throws(typeof(ArgumentNullException), () => { globalOptions.LoadFromFile(ini); });

                string iniFN = null;
                Assert.Throws(typeof(ArgumentNullException), () => { globalOptions.SaveToFile(iniFN); });
                Assert.Throws(typeof(ArgumentNullException), () => { globalOptions.LoadFromFile(iniFN); });

                iniFN = TestUtils.GetTempFilePath("options.ini", out _);
                try {
                    globalOptions.SaveToFile(iniFN);
                    globalOptions.LoadFromFile(iniFN);
                } finally {
                    TestUtils.RemoveTestFile(iniFN);
                }


                MRUFile mruFile = new MRUFile();
                Assert.IsNotNull(mruFile);

                mruFile = new MRUFile("test.ged");
                Assert.IsNotNull(mruFile);
                Assert.AreEqual(-1, globalOptions.MRUFiles_IndexOf("test.ged"));
                globalOptions.MRUFiles.Add(mruFile);
                Assert.AreEqual(0, globalOptions.MRUFiles_IndexOf("test.ged"));

                mruFile.SaveToFile(iniFile, "xxx");
                mruFile.LoadFromFile(iniFile, "xxx");
                MRUFile.DeleteKeys(iniFile, "xxx");
                Assert.Throws(typeof(ArgumentNullException), () => { mruFile.SaveToFile(null, "xxx"); });
                Assert.Throws(typeof(ArgumentNullException), () => { mruFile.LoadFromFile(null, "xxx"); });
                Assert.Throws(typeof(ArgumentNullException), () => { MRUFile.DeleteKeys(null, "xxx"); });
            }
        }

        [Test]
        public void Test_AncestorsCircleOptions()
        {
            using (IniFile iniFile = new IniFile()) {
                CircleChartOptions circleOptions = new CircleChartOptions();
                Assert.IsNotNull(circleOptions);

                circleOptions.Assign(null);
                circleOptions.Assign(new CircleChartOptions());
                circleOptions.SaveToFile(iniFile);
                circleOptions.LoadFromFile(iniFile);

                Assert.Throws(typeof(ArgumentNullException), () => { circleOptions.SaveToFile(null); });
                Assert.Throws(typeof(ArgumentNullException), () => { circleOptions.LoadFromFile(null); });
            }
        }

        [Test]
        public void Test_PedigreeOptions()
        {
            using (IniFile iniFile = new IniFile()) {
                PedigreeOptions pedigreeOptions = new PedigreeOptions();
                Assert.IsNotNull(pedigreeOptions);

                pedigreeOptions.Assign(null);
                pedigreeOptions.Assign(new PedigreeOptions());
                pedigreeOptions.SaveToFile(iniFile);
                pedigreeOptions.LoadFromFile(iniFile);

                Assert.Throws(typeof(ArgumentNullException), () => { pedigreeOptions.SaveToFile(null); });
                Assert.Throws(typeof(ArgumentNullException), () => { pedigreeOptions.LoadFromFile(null); });
            }
        }

        [Test]
        public void Test_ProxyOptions()
        {
            using (IniFile iniFile = new IniFile()) {
                ProxyOptions proxyOptions = new ProxyOptions();
                Assert.IsNotNull(proxyOptions);

                proxyOptions.Assign(null);
                proxyOptions.Assign(new ProxyOptions());
                proxyOptions.SaveToFile(iniFile);
                proxyOptions.LoadFromFile(iniFile);

                Assert.Throws(typeof(ArgumentNullException), () => { proxyOptions.SaveToFile(null); });
                Assert.Throws(typeof(ArgumentNullException), () => { proxyOptions.LoadFromFile(null); });
            }
        }

        [Test]
        public void Test_TreeChartOptions()
        {
            using (IniFile iniFile = new IniFile()) {
                TreeChartOptions treeChartOptions = new TreeChartOptions();
                Assert.IsNotNull(treeChartOptions);

                treeChartOptions.Assign(null);
                treeChartOptions.Assign(new TreeChartOptions());
                treeChartOptions.SaveToFile(iniFile);
                treeChartOptions.LoadFromFile(iniFile);

                Assert.Throws(typeof(ArgumentNullException), () => { treeChartOptions.SaveToFile(null); });
                Assert.Throws(typeof(ArgumentNullException), () => { treeChartOptions.LoadFromFile(null); });
            }
        }
    }
}
