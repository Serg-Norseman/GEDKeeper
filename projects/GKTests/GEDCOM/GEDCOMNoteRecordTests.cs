/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using NUnit.Framework;

namespace GKCommon.GEDCOM
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
        public void testCommon()
        {
            using (GEDCOMNoteRecord noteRec = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "")) {
                Assert.AreEqual(GEDCOMRecordType.rtNote, noteRec.RecordType);

                noteRec.AddNoteText("text");
                Assert.AreEqual("text", noteRec.Note.Text.Trim());

                Assert.Throws(typeof(ArgumentNullException), () => {
                    noteRec.SetNoteText(null);
                });
                noteRec.SetNoteText("Test text");
                Assert.AreEqual("Test text", noteRec.Note.Text.Trim());

                using (GEDCOMNoteRecord noteRec2 = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "")) {
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
                using (GEDCOMNoteRecord noteRec3 = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "")) {
                    noteRec3.SetNoteText("Test text 3");
                    Assert.AreEqual("Test text 3", noteRec3.Note.Text.Trim());

                    noteRec.MoveTo(noteRec3, false);

                    Assert.AreEqual("Test text 3", noteRec3.Note.Text.Trim());
                }
            }
        }

        [Test]
        public void testGetNote()
        {
            GEDCOMNoteRecord instance = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "This is a test");
            StringList expResult = new StringList("This is a test");
            StringList result = instance.Note;
            Assert.AreEqual(expResult.Text, result.Text);
        }

        [Test]
        public void testSetNote()
        {
            string[] lines = new string [] {
                "This is a test line 1",
                "This is a test line 2",
                "This is a test line 3"
            };
            
            StringList value = new StringList(lines);
            GEDCOMNoteRecord instance = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "");
            instance.Note = value;
            
            StringList result = instance.Note;
            Assert.AreEqual(value.Text, result.Text);
        }

        [Test]
        public void testMoveTo1()
        {
            GEDCOMRecord other = new GEDCOMLocationRecord(null, null, "", "");
            GEDCOMNoteRecord instance = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "");
            bool clearDest = false;
            
            Assert.Throws(typeof(ArgumentException), () => {
                instance.MoveTo(other, clearDest);
            });
        }

        [Test]
        public void testMoveTo2()
        {
            string[] lines = new string[] {
                "This is a test line 1",
                "This is a test line 2",
                "This is a test line 3"
            };
            
            string text = "This is a test";
            GEDCOMNoteRecord instance1 = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "BLAH", text);
            GEDCOMNoteRecord instance2 = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "BLUM", "");
            instance2.SetNotesArray(lines);
            bool clearDest = false;
            
            instance1.MoveTo(instance2, clearDest);

            // moveTo preserved existing note text
            StringList value = new StringList(lines);
            StringList result = instance2.Note;
            Assert.AreEqual(value.Text, result.Text);
        }
        
        [Test]
        public void testIsMatch()
        {
            var matchParams = new MatchParams();
            GEDCOMTag other = GEDCOMAddress.Create(null, null, "", "");
            GEDCOMNoteRecord instance = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "");
            float expResult = 0.0F;
            float result = instance.IsMatch(other, matchParams); // TODO matchParams is not used
            Assert.AreEqual(expResult, result, 0.0);
        }

        [Test]
        public void testIsMatch2()
        {
            var matchParams = new MatchParams();
            GEDCOMNoteRecord instance1 = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "This is a test");
            GEDCOMNoteRecord instance2 = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "tHiS iS nOt A tEsT");
            float expResult = 0.0F;
            float result = instance1.IsMatch(instance2, matchParams); // TODO matchParams is not used
            Assert.AreEqual(expResult, result, 0.0);
        }

        [Test]
        public void testIsMatch3()
        {
            var matchParams = new MatchParams();
            GEDCOMNoteRecord instance1 = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "This is a test");
            GEDCOMNoteRecord instance2 = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "This is a test");
            float expResult = 100.0F;
            float result = instance1.IsMatch(instance2, matchParams); // TODO matchParams is not used
            Assert.AreEqual(expResult, result, 0.0);
        }
        
        [Test]
        public void testSetNotesArray()
        {
            string[] lines = new string [] {
                "This is a test line 1",
                "This is a test line 2",
                "This is a test line 3"
            };
            
            GEDCOMNoteRecord instance = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "");
            instance.SetNotesArray(lines);
            
            StringList value = new StringList(lines);
            StringList result = instance.Note;
            Assert.AreEqual(value.Text, result.Text);
        }

        [Test]
        public void testAddNoteText1()
        {
            string text = "This is a test";
            GEDCOMNoteRecord instance = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "");
            instance.AddNoteText(text);

            StringList value = new StringList(text);
            StringList result = instance.Note;
            Assert.AreEqual(value.Text, result.Text);
        }

        [Test]
        public void testAddNoteText2()
        {
            string text1 = "This is a test";
            string text2 = "This is another test";
            GEDCOMNoteRecord instance = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", text1);
            instance.AddNoteText(text2);
            
            StringList value = new StringList(text1);
            value.Add(text2);
            StringList result = instance.Note;
            Assert.AreEqual(value.Text, result.Text);
        }
        
        [Test]
        public void testSetNoteText1()
        {
            string text = "Yet another test";
            GEDCOMNoteRecord instance = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "");
            instance.SetNoteText(text);
            
            StringList value = new StringList(text);
            StringList result = instance.Note;
            Assert.AreEqual(value.Text, result.Text);
        }

        [Test]
        public void testSetNoteText2()
        {
            string text = "Yet another test";
            string text0 = "Initial text";
            GEDCOMNoteRecord instance = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", text0);
            instance.SetNoteText(text);
            
            StringList value = new StringList(text);
            StringList result = instance.Note;
            Assert.AreEqual(value.Text, result.Text);
        }

        [Test]
        public void testSetNoteText3()
        {
            GEDCOMNoteRecord instance = (GEDCOMNoteRecord)GEDCOMNoteRecord.Create(null, null, "", "");
            Assert.Throws(typeof(ArgumentNullException), () => {
                instance.SetNoteText(null);
            });
        }
    }
}