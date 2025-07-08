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

using System;
using BSLib;
using GKCore;
using GKCore.Types;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMNoteRecordTests
    {
        private readonly BaseContext fContext;

        public GDMNoteRecordTests()
        {
            TestUtils.InitGEDCOMProviderTest();
            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            using (GDMNoteRecord noteRec = new GDMNoteRecord(null)) {
                Assert.AreEqual(GDMRecordType.rtNote, noteRec.RecordType);

                noteRec.AddNoteText("text");
                Assert.AreEqual("text", noteRec.Lines.Text.Trim());

                Assert.Throws(typeof(ArgumentNullException), () => {
                    noteRec.SetNoteText(null);
                });

                noteRec.SetNoteText("Test text");
                Assert.AreEqual("Test text", noteRec.Lines.Text.Trim());

                using (GDMNoteRecord noteRec2 = new GDMNoteRecord(null)) {
                    noteRec2.SetNoteText("Test text");
                    Assert.AreEqual("Test text", noteRec2.Lines.Text.Trim());

                    Assert.AreEqual(100.0f, noteRec.IsMatch(noteRec2, new MatchParams()), 0.01f);

                    Assert.IsFalse(noteRec2.IsEmpty());
                    noteRec2.Clear();
                    Assert.IsTrue(noteRec2.IsEmpty());

                    Assert.AreEqual(0.0f, noteRec.IsMatch(noteRec2, new MatchParams()), 0.01f);

                    Assert.AreEqual(0.0f, noteRec.IsMatch(null, new MatchParams()), 0.01f);
                }

                Assert.Throws(typeof(ArgumentException), () => {
                    noteRec.MoveTo(null);
                });

                Assert.Throws(typeof(ArgumentException), () => {
                    noteRec.Assign(null);
                });

                using (GDMNoteRecord noteRec3 = new GDMNoteRecord(null)) {
                    noteRec3.SetNoteText("Test text 3");
                    Assert.AreEqual("Test text 3", noteRec3.Lines.Text.Trim());

                    noteRec.MoveTo(noteRec3);

                    Assert.AreEqual("Test text 3", noteRec3.Lines.Text.Trim());
                }
            }
        }

        [Test]
        public void Test_Common2()
        {
            using (GDMNoteRecord noteRec = new GDMNoteRecord(null)) {
                noteRec.AddNoteText("text");
                Assert.AreEqual("text", noteRec.Lines.Text.Trim());

                Assert.Throws(typeof(ArgumentNullException), () => { noteRec.SetNoteText(null); });

                noteRec.SetNoteText("Test text");
                Assert.AreEqual("Test text", noteRec.Lines.Text.Trim());

                using (GDMNoteRecord noteRec2 = new GDMNoteRecord(null)) {
                    noteRec2.SetNoteText("Test text");
                    Assert.AreEqual("Test text", noteRec2.Lines.Text.Trim());

                    Assert.AreEqual(100.0f, noteRec.IsMatch(noteRec2, new MatchParams()));

                    Assert.IsFalse(noteRec2.IsEmpty());
                    noteRec2.Clear();
                    Assert.IsTrue(noteRec2.IsEmpty());

                    Assert.AreEqual(0.0f, noteRec.IsMatch(noteRec2, new MatchParams()));

                    Assert.AreEqual(0.0f, noteRec.IsMatch(null, new MatchParams()));
                }

                Assert.Throws(typeof(ArgumentException), () => { noteRec.MoveTo(null); });

                using (GDMNoteRecord noteRec3 = new GDMNoteRecord(null)) {
                    noteRec3.SetNoteText("Test text 3");
                    Assert.AreEqual("Test text 3", noteRec3.Lines.Text.Trim());

                    noteRec.MoveTo(noteRec3);

                    Assert.AreEqual("Test text 3", noteRec3.Lines.Text.Trim());
                }
            }
        }

        [Test]
        public void Test_Common3()
        {
            GDMNoteRecord noteRec = fContext.Tree.CreateNote();
            GDMIndividualRecord indiv = fContext.Tree.CreateIndividual();

            noteRec.SetNotesArray(new string[] { "This", "notes", "test" });

            string ctx = GKUtils.MergeStrings(noteRec.Lines);
            Assert.AreEqual("This notes test", ctx);

            noteRec.Lines.Text = "This\r\nnotes2\r\ntest2";
            Assert.AreEqual("This", noteRec.Lines[0]);
            Assert.AreEqual("notes2", noteRec.Lines[1]);
            Assert.AreEqual("test2", noteRec.Lines[2]);

            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.MergeStrings(null); });

            ctx = GKUtils.MergeStrings(noteRec.Lines);
            Assert.AreEqual("This notes2 test2", ctx);

            noteRec.Clear();
            noteRec.AddNoteText("Test text");
            Assert.AreEqual("Test text", noteRec.Lines.Text.Trim());

            GEDCOMNotesTest(noteRec, indiv);

            Assert.IsFalse(noteRec.IsEmpty());
            noteRec.Clear();
            Assert.IsTrue(noteRec.IsEmpty());
        }

        private static void GEDCOMNotesTest(GDMNoteRecord noteRec, GDMIndividualRecord indiv)
        {
            GDMNotes notes = indiv.AddNote(noteRec);

            Assert.IsTrue(notes.IsPointer, "notes.IsPointer");

            Assert.Throws<InvalidOperationException>(() => {
                var lines = notes.Lines;
            }, "Notes is a pointer");

            Assert.AreEqual(notes.XRef, noteRec.XRef);

            Assert.IsFalse(notes.IsEmpty()); // its pointer

            notes.Clear();
        }

        [Test]
        public void Test_GEDCOMNotes()
        {
            using (GDMNotes notes = new GDMNotes()) {
                Assert.IsTrue(notes.IsEmpty());
                notes.Lines.Text = "Test note";
                Assert.IsFalse(notes.IsEmpty());
                Assert.AreEqual("Test note", notes.Lines.Text);
            }
        }


        [Test]
        public void Test_GetNote()
        {
            GDMNoteRecord instance = new GDMNoteRecord(null);
            instance.ParseString("This is a test");
            StringList expResult = new StringList("This is a test");
            Assert.AreEqual(expResult.Text, instance.Lines.Text);
        }

        [Test]
        public void Test_SetNote()
        {
            string[] lines = new string [] {
                "This is a test line 1",
                "This is a test line 2",
                "This is a test line 3"
            };
            
            StringList value = new StringList(lines);
            GDMNoteRecord instance = new GDMNoteRecord(null);
            instance.Lines.AddRange(lines);
            Assert.AreEqual(value.Text, instance.Lines.Text);
        }

        [Test]
        public void Test_MoveTo1()
        {
            GDMRecord other = new GDMLocationRecord(null);
            GDMNoteRecord instance = new GDMNoteRecord(null);

            Assert.Throws(typeof(ArgumentException), () => {
                instance.MoveTo(other);
            });
        }

        [Test]
        public void Test_MoveTo2()
        {
            string[] lines = new string[] {
                "This is a test line 1",
                "This is a test line 2",
                "This is a test line 3"
            };

            string text = "This is a test";

            GDMNoteRecord instance1 = new GDMNoteRecord(null);
            instance1.ParseString(text);

            GDMNoteRecord instance2 = new GDMNoteRecord(null);
            instance2.SetNotesArray(lines);

            instance1.MoveTo(instance2);

            // moveTo preserved existing note text
            StringList value = new StringList(lines);
            Assert.AreEqual(value.Text, instance2.Lines.Text);
        }

        [Test]
        public void Test_IsMatch()
        {
            var matchParams = new MatchParams();
            GDMTag other = new GDMAddress();
            GDMNoteRecord instance = new GDMNoteRecord(null);
            float result = instance.IsMatch(other, matchParams); // TODO matchParams is not used
            Assert.AreEqual(0.0F, result, 0.0);
        }

        [Test]
        public void Test_IsMatch2()
        {
            var matchParams = new MatchParams();

            GDMNoteRecord instance1 = new GDMNoteRecord(null);
            instance1.ParseString("This is a test");

            GDMNoteRecord instance2 = new GDMNoteRecord(null);
            instance2.ParseString("tHiS iS nOt A tEsT");

            float result = instance1.IsMatch(instance2, matchParams); // TODO matchParams is not used
            Assert.AreEqual(0.0F, result, 0.0);
        }

        [Test]
        public void Test_IsMatch3()
        {
            var matchParams = new MatchParams();

            GDMNoteRecord instance1 = new GDMNoteRecord(null);
            instance1.ParseString("This is a test");

            GDMNoteRecord instance2 = new GDMNoteRecord(null);
            instance2.ParseString("This is a test");

            float result = instance1.IsMatch(instance2, matchParams); // TODO matchParams is not used
            Assert.AreEqual(100.0F, result, 0.0);
        }
        
        [Test]
        public void Test_SetNotesArray()
        {
            string[] lines = new string [] {
                "This is a test line 1",
                "This is a test line 2",
                "This is a test line 3"
            };
            
            GDMNoteRecord instance = new GDMNoteRecord(null);
            instance.SetNotesArray(lines);
            
            StringList value = new StringList(lines);
            Assert.AreEqual(value.Text, instance.Lines.Text);
        }

        [Test]
        public void Test_AddNoteText1()
        {
            string text = "This is a test";
            GDMNoteRecord instance = new GDMNoteRecord(null);
            instance.AddNoteText(text);

            StringList value = new StringList(text);
            Assert.AreEqual(value.Text, instance.Lines.Text);
        }

        [Test]
        public void Test_AddNoteText2()
        {
            string text1 = "This is a test";
            string text2 = "This is another test";

            GDMNoteRecord instance = new GDMNoteRecord(null);
            instance.ParseString(text1);

            instance.AddNoteText(text2);
            
            StringList value = new StringList(text1);
            value.Add(text2);
            Assert.AreEqual(value.Text, instance.Lines.Text);
        }
        
        [Test]
        public void Test_SetNoteText1()
        {
            string text = "Yet another test";
            GDMNoteRecord instance = new GDMNoteRecord(null);
            instance.SetNoteText(text);
            
            StringList value = new StringList(text);
            Assert.AreEqual(value.Text, instance.Lines.Text);
        }

        [Test]
        public void Test_SetNoteText2()
        {
            string text = "Yet another test";
            string text0 = "Initial text";

            GDMNoteRecord instance = new GDMNoteRecord(null);
            instance.ParseString(text0);
            instance.SetNoteText(text);
            
            StringList value = new StringList(text);
            Assert.AreEqual(value.Text, instance.Lines.Text);
        }

        [Test]
        public void Test_SetNoteText3()
        {
            GDMNoteRecord instance = new GDMNoteRecord(null);
            Assert.Throws(typeof(ArgumentNullException), () => {
                instance.SetNoteText(null);
            });
        }
    }
}
