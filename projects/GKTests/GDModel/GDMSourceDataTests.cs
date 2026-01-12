/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel.Providers.GEDCOM;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMSourceDataTests
    {
        [Test]
        public void Test_Common()
        {
            using (GDMSourceData data = new GDMSourceData()) {
                Assert.IsNotNull(data);

                data.Agency = "test agency";
                Assert.AreEqual("test agency", data.Agency);

                GDMTag evenTag = data.Events.Add(new GDMSourceEvent());
                Assert.IsNotNull(evenTag);

                GDMSourceEvent evt = data.Events[0];
                Assert.AreEqual(evenTag, evt);

                evt.StringValue = "BIRT";

                var note = new GDMNotes();
                note.Lines.Text = "test sourcedata notes";
                data.Notes.Add(note);

                string buf = GEDCOMProvider.GetTagStreamText(data, 0);
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
            using (GDMSourceEvent evt = new GDMSourceEvent()) {
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
