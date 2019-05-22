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

using System;
using BSLib;
using GDModel;
using GKCore.Types;
using NUnit.Framework;

namespace GDModel
{
    /**
     *
     * @author Sergey V. Zhdanovskih
     * Modified by Kevin Routley (KBR) aka fire-eggs
     */
    [TestFixture]
    public class GEDCOMNoteRecordTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMNoteRecord noteRec = new GDMNoteRecord(null)) {
                Assert.AreEqual(GEDCOMRecordType.rtNote, noteRec.RecordType);

                noteRec.AddNoteText("text");
                Assert.AreEqual("text", noteRec.Note.Text.Trim());

                Assert.Throws(typeof(ArgumentNullException), () => {
                    noteRec.SetNoteText(null);
                });
                noteRec.SetNoteText("Test text");
                Assert.AreEqual("Test text", noteRec.Note.Text.Trim());

                using (GDMNoteRecord noteRec2 = new GDMNoteRecord(null)) {
                    noteRec2.SetNoteText("Test text");
                    Assert.AreEqual("Test text", noteRec2.Note.Text.Trim());

                    Assert.AreEqual(100.0f, noteRec.IsMatch(noteRec2, new MatchParams()), 0.01f);

                    Assert.IsFalse(noteRec2.IsEmpty());
                    noteRec2.Clear();
                    Assert.IsTrue(noteRec2.IsEmpty());

                    Assert.AreEqual(0.0f, noteRec.IsMatch(noteRec2, new MatchParams()), 0.01f);

                    Assert.AreEqual(0.0f, noteRec.IsMatch(null, new MatchParams()), 0.01f);
                }

                Assert.Throws(typeof(ArgumentException), () => {
                    noteRec.MoveTo(null, false);
                });
                using (GDMNoteRecord noteRec3 = new GDMNoteRecord(null)) {
                    noteRec3.SetNoteText("Test text 3");
                    Assert.AreEqual("Test text 3", noteRec3.Note.Text.Trim());

                    noteRec.MoveTo(noteRec3, false);

                    Assert.AreEqual("Test text 3", noteRec3.Note.Text.Trim());
                }
            }
        }

        [Test]
        public void Test_GetNote()
        {
            GDMNoteRecord instance = new GDMNoteRecord(null);
            instance.ParseString("This is a test");
            StringList expResult = new StringList("This is a test");
            Assert.AreEqual(expResult.Text, instance.Note.Text);
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
            instance.Note = value;
            Assert.AreEqual(value.Text, instance.Note.Text);
        }

        [Test]
        public void Test_MoveTo1()
        {
            GDMRecord other = new GDMLocationRecord(null);
            GDMNoteRecord instance = new GDMNoteRecord(null);
            bool clearDest = false;
            
            Assert.Throws(typeof(ArgumentException), () => {
                instance.MoveTo(other, clearDest);
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
            bool clearDest = false;
            
            instance1.MoveTo(instance2, clearDest);

            // moveTo preserved existing note text
            StringList value = new StringList(lines);
            Assert.AreEqual(value.Text, instance2.Note.Text);
        }
        
        [Test]
        public void Test_IsMatch()
        {
            var matchParams = new MatchParams();
            GDMTag other = new GDMAddress(null);
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
            Assert.AreEqual(value.Text, instance.Note.Text);
        }

        [Test]
        public void Test_AddNoteText1()
        {
            string text = "This is a test";
            GDMNoteRecord instance = new GDMNoteRecord(null);
            instance.AddNoteText(text);

            StringList value = new StringList(text);
            Assert.AreEqual(value.Text, instance.Note.Text);
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
            Assert.AreEqual(value.Text, instance.Note.Text);
        }
        
        [Test]
        public void Test_SetNoteText1()
        {
            string text = "Yet another test";
            GDMNoteRecord instance = new GDMNoteRecord(null);
            instance.SetNoteText(text);
            
            StringList value = new StringList(text);
            Assert.AreEqual(value.Text, instance.Note.Text);
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
            Assert.AreEqual(value.Text, instance.Note.Text);
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