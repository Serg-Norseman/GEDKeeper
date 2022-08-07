/*
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

using System;
using GDModel.Providers.GEDCOM;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMPlaceTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMPlace place = new GDMPlace()) {
                Assert.IsNotNull(place);

                place.Form = "abrakadabra";
                Assert.AreEqual("abrakadabra", place.Form);

                Assert.IsNotNull(place.Map);
                Assert.IsNotNull(place.Location);

                var note = new GDMNotes();
                note.Lines.Text = "place notes";
                place.Notes.Add(note);

                using (GDMPlace place2 = new GDMPlace()) {
                    Assert.Throws(typeof(ArgumentException), () => {
                        place2.Assign(null);
                    });

                    var iRec = new GDMIndividualRecord(null);
                    var evt = new GDMIndividualEvent();
                    evt.SetName("BIRT");
                    iRec.Events.Add(evt);
                    //place2.Assign(place);
                    evt.Place.Assign(place);

                    string buf = GEDCOMProvider.GetTagStreamText(iRec, 1);
                    Assert.AreEqual("0 INDI\r\n" +
                                    "1 SEX U\r\n" +
                                    "1 BIRT\r\n" +
                                    "2 PLAC\r\n" +
                                    "3 NOTE place notes\r\n" +
                                    "3 FORM abrakadabra\r\n", buf);
                }

                place.ReplaceXRefs(new GDMXRefReplacer());

                Assert.IsFalse(place.IsEmpty());
                place.Clear();
                Assert.IsTrue(place.IsEmpty());
            }
        }
    }
}
