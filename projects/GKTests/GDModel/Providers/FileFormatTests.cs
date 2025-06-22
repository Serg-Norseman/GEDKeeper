/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using System.IO;
using System.Text;
using GDModel.Providers.FamilyShow;
using GDModel.Providers.GEDCOM;
using GDModel.Providers.GedML;
using GKCore;
using GKCore.Interfaces;
using GKTests;
using NSubstitute;
using NUnit.Framework;

namespace GDModel.Providers
{
    [TestFixture]
    public class FileFormatTests
    {
        public FileFormatTests()
        {
            // for tests on net6.0 needs instance of AppHost
            TestUtils.InitUITest();
        }

        [Test]
        public void Test_NativeFormat()
        {
            GEDCOMProvider.DebugWrite = false;
            using (Stream inStream = TestUtils.LoadResourceStream("test_native.ged")) {
                using (GDMTree tree = new GDMTree()) {
                    byte[] inArray;
                    using (MemoryStream inMem = new MemoryStream()) {
                        inStream.CopyTo(inMem);
                        inStream.Position = 0;
                        inArray = inMem.ToArray();
                    }

                    var gedcomProvider = new GEDCOMProvider(tree, true, false);
                    gedcomProvider.LoadFromStreamExt(inStream, inStream);

                    using (MemoryStream outStream = new MemoryStream()) {
                        gedcomProvider = new GEDCOMProvider(tree);
                        gedcomProvider.SaveToStreamExt(outStream, GEDCOMCharacterSet.csUTF8);

                        outStream.Position = 0;
                        byte[] outArray = outStream.ToArray();

                        string inStr = Encoding.ASCII.GetString(inArray);
                        // convert to GK GEDCOM
                        inStr = inStr.Replace("1 ALIA @I11@", "1 ASSO @I11@\r\n2 RELA possible_duplicate");

                        string outStr = Encoding.ASCII.GetString(outArray);
                        Assert.AreEqual(inStr, outStr);
                    }
                }
            }
        }

        [Test]
        public void Test_Standart()
        {
            using (Stream inStream = TestUtils.LoadResourceStream("TGC55CLF.GED")) {
                using (GDMTree tree = new GDMTree()) {
                    var gedcomProvider = new GEDCOMProvider(tree);
                    gedcomProvider.LoadFromStreamExt(inStream, inStream);

                    Assert.AreEqual(GEDCOMFormat.Unknown, tree.Format);

                    GDMMultimediaRecord mmRec1 = tree.XRefIndex_Find("M1") as GDMMultimediaRecord;
                    Assert.IsNotNull(mmRec1);

                    GDMTag blobTag = mmRec1.FindTag("BLOB", 0);
                    Assert.IsNotNull(blobTag);
                    var strBlob = GEDCOMUtils.GetTagStrings(blobTag).Text;
                    Assert.IsNotNull(strBlob);
                    MemoryStream blobStream = GEDCOMUtils.DecodeBlob(strBlob.Replace("\r\n", ""));
                    Assert.IsNotNull(blobStream);

                    // unsupported apple's `pict` format
                    /*using (FileStream file = new FileStream("d:\\image.pict", FileMode.Create, FileAccess.Write)) {
                        blobStream.WriteTo(file);
                        blobStream.Close();
                    }*/

                    using (MemoryStream outStream = new MemoryStream()) {
                        gedcomProvider = new GEDCOMProvider(tree);
                        gedcomProvider.SaveToStreamExt(outStream, GEDCOMCharacterSet.csASCII);
                    }
                }
            }
        }

        [Test]
        public void Test_GK_UTF8()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                using (Stream stmGed1 = TestUtils.LoadResourceStream("test_gk_utf8.ged")) {
                    var charsetRes = GKUtils.DetectCharset(stmGed1);
                    Assert.AreEqual("utf-8", charsetRes.Charset);
                    Assert.AreEqual(1.0f, charsetRes.Confidence);

                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    Assert.AreEqual(GEDCOMFormat.Native, ctx.Tree.Format);

                    GDMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                    Assert.IsNotNull(iRec1);

                    Assert.AreEqual("Иван Иванович Иванов", iRec1.GetPrimaryFullName());
                }
            }
        }

        [Test]
        public void Test_gedcom4j_UTF8()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                using (Stream stmGed1 = TestUtils.LoadResourceStream("test_gc4j_utf8.ged")) {
                    var charsetRes = GKUtils.DetectCharset(stmGed1);
                    Assert.AreEqual("utf-8", charsetRes.Charset);
                    Assert.AreEqual(1.0f, charsetRes.Confidence);

                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1, true);

                    Assert.AreEqual(GEDCOMFormat.gedcom4j, ctx.Tree.Format);

                    var iRec1 = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                    Assert.IsNotNull(iRec1);
                    Assert.AreEqual("John Grüber", iRec1.GetPrimaryFullName());
                }
            }
        }

        [Test]
        public void Test_Ahnenblatt_ANSI_Win1250()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                using (Stream stmGed1 = TestUtils.LoadResourceStream("test_ahn_ansi(win1250).ged")) {
                    var charsetRes = GKUtils.DetectCharset(stmGed1);
                    Assert.AreEqual("windows-1250", charsetRes.Charset);
                    Assert.GreaterOrEqual(charsetRes.Confidence, 0.77f);

                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    Assert.AreEqual(GEDCOMFormat.Ahnenblatt, ctx.Tree.Format);

                    GDMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                    Assert.IsNotNull(iRec1);

                    Assert.AreEqual("Stanisław Nowak", iRec1.GetPrimaryFullName());
                }
            }
        }

        [Test]
        public void Test_Agelong_PseudoAnsel_Win1251()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                using (Stream stmGed1 = TestUtils.LoadResourceStream("test_agelong_ansel(win1251).ged")) {
                    var charsetRes = GKUtils.DetectCharset(stmGed1);
                    Assert.AreEqual("windows-1251", charsetRes.Charset);
                    Assert.GreaterOrEqual(charsetRes.Confidence, 0.46f);

                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    Assert.AreEqual(GEDCOMFormat.ALTREE, ctx.Tree.Format);

                    GDMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                    Assert.IsNotNull(iRec1);

                    Assert.AreEqual("Иван ОВЕЧКИН", iRec1.GetPrimaryFullName());
                }
            }
        }

        [Test]
        public void Test_FTB6_ANSI_Win1251()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                using (Stream stmGed1 = TestUtils.LoadResourceStream("test_ftb6_ansi(win1251).ged")) {
                    var charsetRes = GKUtils.DetectCharset(stmGed1);
                    Assert.AreEqual("windows-1251", charsetRes.Charset);
                    Assert.GreaterOrEqual(charsetRes.Confidence, 0.69f);

                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    Assert.AreEqual(GEDCOMFormat.FTB, ctx.Tree.Format);

                    GDMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                    Assert.IsNotNull(iRec1);

                    Assert.AreEqual("Иван Васильевич Петров", iRec1.GetPrimaryFullName());
                }
            }
        }

        [Test]
        public void Test_Notes_StdGEDCOM()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_notes_stdgedcom.ged")) {
                Assert.AreEqual(GEDCOMFormat.Native, ctx.Tree.Format);

                GDMNoteRecord noteRec1 = ctx.Tree.XRefIndex_Find("N1") as GDMNoteRecord;
                Assert.IsNotNull(noteRec1);
                Assert.AreEqual("Test1\r\ntest2\r\ntest3", noteRec1.Lines.Text);

                GDMNoteRecord noteRec2 = ctx.Tree.XRefIndex_Find("N2") as GDMNoteRecord;
                Assert.IsNotNull(noteRec2);
                Assert.AreEqual("Test\r\ntest2\r\ntest3", noteRec2.Lines.Text);
            }
        }

        [Test]
        public void Test_Notes_LeadingSpaces()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_notes_leadspaces.ged")) {
                Assert.AreEqual(GEDCOMFormat.Native, ctx.Tree.Format);

                GDMNoteRecord noteRec1 = ctx.Tree.XRefIndex_Find("N1") as GDMNoteRecord;
                Assert.IsNotNull(noteRec1);
                Assert.AreEqual("Test1\r\n    test2\r\n  test3", noteRec1.Lines.Text);

                GDMNoteRecord noteRec2 = ctx.Tree.XRefIndex_Find("N2") as GDMNoteRecord;
                Assert.IsNotNull(noteRec2);
                Assert.AreEqual("    Test\r\n  test2\r\n      test3", noteRec2.Lines.Text);
            }
        }

        [Test]
        public void Test_TrueAnsel()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                using (Stream stmGed1 = TestUtils.LoadResourceStream("test_ansel.ged")) {
                    var charsetRes = GKUtils.DetectCharset(stmGed1);
                    Assert.AreEqual(null, charsetRes.Charset);
                    Assert.GreaterOrEqual(charsetRes.Confidence, 0.0f);

                    Assert.AreEqual(GEDCOMFormat.Unknown, ctx.Tree.Format);

                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);
                }
            }
        }

        [Test]
        public void Test_FTB_BadLine()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_ftb_badline.ged")) {
                Assert.AreEqual(GEDCOMFormat.FTB, ctx.Tree.Format);

                GDMNoteRecord noteRec1 = ctx.Tree.XRefIndex_Find("N1") as GDMNoteRecord;
                Assert.IsNotNull(noteRec1);
                Assert.AreEqual("Test1\r\ntest2\r\ntest3\r\nbadline badline badline badline", noteRec1.Lines.Text);
            }
        }

        [Test]
        public void Test_FTB_BadPosition()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_ftb_badpos.ged")) {
                Assert.AreEqual(GEDCOMFormat.FTB, ctx.Tree.Format);

                //GDMNoteRecord noteRec1 = ctx.Tree.XRefIndex_Find("N1") as GDMNoteRecord;
                //Assert.IsNotNull(noteRec1);
                //Assert.AreEqual("Test1\r\ntest2\r\ntest3\r\nbadline badline badline badline", noteRec1.Lines.Text);
            }
        }

        [Test]
        public void Test_FTB_DamagedUtf8Lines()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_ftb_dmgutf8rus.ged")) {
                Assert.AreEqual(GEDCOMFormat.FTB, ctx.Tree.Format);

                GDMIndividualRecord iRec2 = ctx.Tree.XRefIndex_Find("I2") as GDMIndividualRecord;
                Assert.IsNotNull(iRec2);
                Assert.AreEqual(2, iRec2.SourceCitations.Count);

                var scText = iRec2.SourceCitations[0].Data.Text.Lines.Text;
                Assert.AreEqual("Дети: Дмитрий, Андриян, Яков, Егор Алексеевич, Ефросинья, Ксения, Полина", scText);

                scText = iRec2.SourceCitations[1].Data.Text.Lines.Text;
                Assert.AreEqual("Евдокия ПоликарповнаФамилия после замужества: КуроваРодители: Поликарп, Екатерина", scText);

                GDMIndividualRecord iRec7 = ctx.Tree.XRefIndex_Find("I7") as GDMIndividualRecord;
                Assert.IsNotNull(iRec7);
                Assert.AreEqual(2, iRec7.SourceCitations.Count);

                scText = iRec7.SourceCitations[0].Data.Text.Lines.Text;
                Assert.AreEqual("Рождение: Титово, Дороховская волость, Богородский уезд, Московская губерния", scText);

                scText = iRec7.SourceCitations[1].Data.Text.Lines.Text;
                Assert.AreEqual("Родные брат/сестра: Григорий, Анастасия, Гавриил, Саввин, Яков Федорович", scText);
            }
        }

        [Test]
        public void Test_MinIndented()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_min_indented.ged")) {
                Assert.AreEqual(GEDCOMFormat.Unknown, ctx.Tree.Format);

                var subm = ctx.Tree.GetPtrValue<GDMSubmitterRecord>(ctx.Tree.Header.Submitter);
                Assert.IsNotNull(subm);
                Assert.AreEqual("John Doe", subm.Name);
            }
        }

        [Test]
        public void Test_EmptyLines()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_empty_lines.ged")) {
                Assert.AreEqual(GEDCOMFormat.Unknown, ctx.Tree.Format);

                GDMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I001") as GDMIndividualRecord;
                Assert.IsNotNull(iRec1);
                Assert.AreEqual("John Doe", iRec1.GetPrimaryFullName());

                GDMIndividualRecord iRec2 = ctx.Tree.XRefIndex_Find("I002") as GDMIndividualRecord;
                Assert.IsNotNull(iRec2);
                Assert.AreEqual("Jane Doe", iRec2.GetPrimaryFullName());
            }
        }

        [Test]
        public void Test_FamilyHistorian()
        {
            var progress = Substitute.For<IProgressController>();

            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_famhist.ged")) {
                GEDCOMChecker.CheckGEDCOMFormat(ctx, progress);

                Assert.AreEqual(GEDCOMFormat.FamilyHistorian, ctx.Tree.Format);

                GDMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                Assert.IsNotNull(iRec1);
                Assert.AreEqual("Tom Thompson", iRec1.GetPrimaryFullName());
            }
        }

        [Test]
        public void Test_FamilyHistorian2()
        {
            var progress = Substitute.For<IProgressController>();

            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_famhist2.ged")) {
                GEDCOMChecker.CheckGEDCOMFormat(ctx, progress);

                Assert.AreEqual(GEDCOMFormat.FamilyHistorian, ctx.Tree.Format);

                GDMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                Assert.IsNotNull(iRec1);
                Assert.AreEqual("Tom Thompson", iRec1.GetPrimaryFullName());

                var locRec1 = ctx.Tree.XRefIndex_Find("L2") as GDMLocationRecord; // Pn -> Ln
                Assert.IsNotNull(locRec1);
                Assert.AreEqual("Lima, Peru", locRec1.LocationName);
            }
        }

        [Test]
        public void Test_FamilyTreeMaker_2008()
        {
            var progress = Substitute.For<IProgressController>();

            // actually need to find the real signature of version for FTM 2008 (wrong in the file)
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_ftm2008.ged")) {
                Assert.AreEqual(GEDCOMFormat.FamilyTreeMaker, ctx.Tree.Format);

                GEDCOMChecker.CheckGEDCOMFormat(ctx, progress);

                GDMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                Assert.IsNotNull(iRec1);
                Assert.AreEqual("John Smith", iRec1.GetPrimaryFullName());

                // test of conversion
                var occuEvent = iRec1.FindEvent(GEDCOMTagType.OCCU);
                Assert.IsNotNull(occuEvent);
                Assert.AreEqual("test occupation", occuEvent.StringValue);
            }
        }

        [Test]
        public void Test_NoteLongLine()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_longline.ged")) {
                Assert.AreEqual(GEDCOMFormat.Unknown, ctx.Tree.Format);

                GDMNoteRecord noteRec1 = ctx.Tree.XRefIndex_Find("N1") as GDMNoteRecord;
                Assert.IsNotNull(noteRec1);
                string str = noteRec1.Lines.Text;
                Assert.IsTrue(str.EndsWith("non-standard GEDCOM files."));
            }
        }

        [Test]
        public void Test_Address_CONT()
        {
            var progress = Substitute.For<IProgressController>();
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_addr_cont.ged")) {
                GEDCOMChecker.CheckGEDCOMFormat(ctx, progress);
                var repoRec = ctx.Tree.XRefIndex_Find("R1") as GDMRepositoryRecord;
                Assert.IsNotNull(repoRec);
                Assert.AreEqual("line 1\r\nline 2\r\nline 3", repoRec.Address.Lines.Text);
            }
        }

        [Test]
        public void Test_Address_123()
        {
            var progress = Substitute.For<IProgressController>();
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_addr123.ged")) {
                GEDCOMChecker.CheckGEDCOMFormat(ctx, progress);
                var repoRec = ctx.Tree.XRefIndex_Find("R1") as GDMRepositoryRecord;
                Assert.IsNotNull(repoRec);
                Assert.AreEqual("line 1\r\nline 2\r\nline 3", repoRec.Address.Lines.Text);
            }
        }

        [Test]
        public void Test_Heredis()
        {
            // TODO: interest feature - use PLAC.FORM
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_heredis.ged")) {
                Assert.AreEqual(GEDCOMFormat.Heredis, ctx.Tree.Format);
            }
        }

        [Test]
        public void Test_AncestQuest()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_aq.ged")) {
                Assert.AreEqual(GEDCOMFormat.AncestQuest, ctx.Tree.Format);
            }
        }

        [Test]
        public void Test_Geni()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_geni.ged")) {
                Assert.AreEqual(GEDCOMFormat.Geni, ctx.Tree.Format);
            }
        }

        [Test]
        public void Test_RootsMagic()
        {
            var progress = Substitute.For<IProgressController>();

            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_rootsmagic.ged")) {
                Assert.AreEqual(GEDCOMFormat.RootsMagic, ctx.Tree.Format);

                GEDCOMChecker.CheckGEDCOMFormat(ctx, progress);
            }
        }

        [Test]
        public void Test_Geni_Badlines()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_geni_badlines.ged")) {
                Assert.AreEqual(GEDCOMFormat.Geni, ctx.Tree.Format);

                var iRec1 = ctx.Tree.XRefIndex_Find("I500000006275123389") as GDMIndividualRecord;
                Assert.IsNotNull(iRec1);

                Assert.AreEqual(1, iRec1.Notes.Count);
                var note = iRec1.Notes[0];
                Assert.AreEqual("{geni:about_me} '''Jane Doe'''\r\nBirth at 1955, Raccoon City; \r\n\r\n\r\nbadline 1\r\nbadline 2\r\nbadline 3", note.Lines.Text);
            }
        }

        [Test]
        public void Test_Geni_SourceCitations()
        {
            var progress = Substitute.For<IProgressController>();

            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_geni_srcit.ged")) {
                GEDCOMChecker.CheckGEDCOMFormat(ctx, progress);

                Assert.AreEqual(GEDCOMFormat.Geni, ctx.Tree.Format);

                var iRec1 = ctx.Tree.XRefIndex_Find("I6000000012345678912") as GDMIndividualRecord;
                Assert.IsNotNull(iRec1);

                Assert.AreEqual(3, iRec1.SourceCitations.Count);

                var srCit = iRec1.SourceCitations[0];
                Assert.IsTrue(srCit.IsPointer);
                Assert.AreEqual("21 APR 2020", srCit.Data.Date.StringValue);
                var sourceRec = ctx.Tree.GetPtrValue<GDMSourceRecord>(srCit);
                Assert.AreEqual(1, sourceRec.MultimediaLinks.Count);

                srCit = iRec1.SourceCitations[1];
                Assert.IsTrue(srCit.IsPointer);
                Assert.AreEqual("23 APR 2020", srCit.Data.Date.StringValue);
                sourceRec = ctx.Tree.GetPtrValue<GDMSourceRecord>(srCit);
                Assert.AreEqual(1, sourceRec.MultimediaLinks.Count);

                srCit = iRec1.SourceCitations[2];
                Assert.IsTrue(srCit.IsPointer);
                Assert.AreEqual("25 APR 2020", srCit.Data.Date.StringValue);
                sourceRec = ctx.Tree.GetPtrValue<GDMSourceRecord>(srCit);
                Assert.AreEqual(1, sourceRec.MultimediaLinks.Count);
            }
        }

        [Test]
        public void Test_Geni_DateYearBC()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_dates_year.ged")) {
                var iRec = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                Assert.IsNotNull(iRec);

                GDMDate dtx;
                GDMCustomEvent evt;

                evt = iRec.FindEvent("BIRT");
                dtx = evt.Date.Value as GDMDate;
                Assert.AreEqual(32, dtx.Year);
                Assert.AreEqual(true, dtx.YearBC);
                Assert.AreEqual("032B.C.", dtx.StringValue);

                evt = iRec.FindEvent("DEAT");
                dtx = evt.Date.Value as GDMDate;
                Assert.AreEqual(32, dtx.Year);
                Assert.AreEqual(false, dtx.YearBC);
                Assert.AreEqual("032", dtx.StringValue);

                iRec = ctx.Tree.XRefIndex_Find("I2") as GDMIndividualRecord;
                Assert.IsNotNull(iRec);

                evt = iRec.FindEvent("BIRT");
                dtx = evt.Date.Value as GDMDate;
                Assert.AreEqual(31, dtx.Year);
                Assert.AreEqual(true, dtx.YearBC);
                Assert.AreEqual("031B.C.", dtx.StringValue);

                evt = iRec.FindEvent("DEAT");
                dtx = evt.Date.Value as GDMDate;
                Assert.AreEqual(31, dtx.Year);
                Assert.AreEqual(false, dtx.YearBC);
                Assert.AreEqual("031", dtx.StringValue);
            }
        }

        [Test]
        public void Test_Geni_NegativeYears()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_geni_neg_years.ged")) {
                Assert.AreEqual(GEDCOMFormat.Geni, ctx.Tree.Format);

                var iRec = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                Assert.IsNotNull(iRec);

                GDMDateRange dtx;
                GDMCustomEvent evt;

                evt = iRec.FindEvent("BIRT");
                dtx = evt.Date.Value as GDMDateRange;
                Assert.IsNotNull(dtx);
                Assert.AreEqual(25, dtx.After.Year);
                Assert.AreEqual(true, dtx.After.YearBC);
                Assert.AreEqual(29, dtx.Before.Year);
                Assert.AreEqual(false, dtx.Before.YearBC);
            }
        }

        [Test]
        public void Test_Legacy()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_legacy.ged")) {
                Assert.AreEqual(GEDCOMFormat.Legacy, ctx.Tree.Format);
            }
        }

        [Test]
        public void Test_EasyTree()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_easytree.ged")) {
                Assert.AreEqual(GEDCOMFormat.EasyTree, ctx.Tree.Format);
            }
        }

        [Test]
        public void Test_Genney()
        {
            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_genney.ged")) {
                Assert.AreEqual(GEDCOMFormat.Genney, ctx.Tree.Format);
            }
        }

        [Test]
        public void Test_Ages_Adopted()
        {
            var progress = Substitute.For<IProgressController>();

            using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_ages_adop.ged")) {
                Assert.AreEqual(GEDCOMFormat.AGES, ctx.Tree.Format);
                GEDCOMChecker.CheckGEDCOMFormat(ctx, progress);

                GDMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I3") as GDMIndividualRecord;
                Assert.IsNotNull(iRec1);

                Assert.AreEqual("2nd son adopted", iRec1.GetPrimaryFullName());

                Assert.AreEqual(1, iRec1.ChildToFamilyLinks.Count);
                var ctfLink = iRec1.ChildToFamilyLinks[0];
                Assert.IsNotNull(ctfLink);
                Assert.AreEqual(GDMPedigreeLinkageType.plAdopted, ctfLink.PedigreeLinkageType);
            }
        }

        [Test]
        public void Test_EmptyFile()
        {
            Assert.Throws(typeof(GEDCOMEmptyFileException), () => {
                using (var ctx = TestUtils.LoadResourceGEDCOMFile("test_empty.ged")) {
                }
            });
        }


        [Test]
        public void Test_GedML()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                using (Stream stmGed1 = TestUtils.LoadResourceStream("test_gedml.xml")) {
                    var gedmlProvider = new GedMLProvider(ctx.Tree);
                    gedmlProvider.LoadFromStreamExt(stmGed1, stmGed1);
                }

                GDMSubmitterRecord submRec = ctx.Tree.XRefIndex_Find("SUB1") as GDMSubmitterRecord;
                Assert.IsNotNull(submRec);
                Assert.AreEqual("Michael Kay", submRec.Name);

                GDMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
                Assert.IsNotNull(iRec1);
                //Assert.AreEqual("John Smith", iRec1.GetPrimaryFullName());
            }
        }

        [Test]
        public void Test_FamilyShow()
        {
            using (Stream inStream = TestUtils.LoadResourceStream("test_windsor.familyx")) {
                using (GDMTree tree = new GDMTree()) {
                    var fxProvider = new FamilyXProvider(tree);
                    fxProvider.LoadFromStreamExt(inStream, inStream);

                    Assert.AreEqual(118, tree.RecordsCount);
                }
            }
        }
    }
}
