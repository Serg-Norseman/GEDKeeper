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

using GDModel;
using GDModel.Providers.GEDCOM;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMSourceDataTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMSourceData data = new GDMSourceData(null)) {
                Assert.IsNotNull(data);

                data.Agency = "test agency";
                Assert.AreEqual("test agency", data.Agency);

                GDMTag evenTag = data.Events.Add(new GDMSourceEvent(data));
                Assert.IsNotNull(evenTag);

                GDMSourceEvent evt = data.Events[0];
                Assert.AreEqual(evenTag, evt);

                evt.StringValue = "BIRT";

                var note = new GDMNotes(data);
                note.Lines.Text = "test sourcedata notes";
                data.Notes.Add(note);

                string buf = TestUtils.GetTagStreamText(data, 0);
                Assert.AreEqual("1 DATA\r\n" +
                                "2 NOTE test sourcedata notes\r\n" +
                                "2 AGNC test agency\r\n" +
                                "2 EVEN BIRT\r\n", buf);

                data.ReplaceXRefs(new GDMXRefReplacer());

                Assert.IsFalse(data.IsEmpty());
                data.Clear();
                Assert.IsTrue(data.IsEmpty());
            }
        }

        [Test]
        public void Test_SourceEvent()
        {
            using (GDMSourceEvent evt = new GDMSourceEvent(null)) {
                Assert.IsNotNull(evt);

                Assert.IsNotNull(evt.Date);

                Assert.IsNotNull(evt.Place);

                evt.Place.StringValue = "test";

                Assert.IsFalse(evt.IsEmpty());
                evt.Clear();
                Assert.IsTrue(evt.IsEmpty());
            }
        }
    }
}
