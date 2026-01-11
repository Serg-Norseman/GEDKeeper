// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

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
