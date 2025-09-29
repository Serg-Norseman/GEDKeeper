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

using System;
using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMTreeTests
    {
        private readonly BaseContext fContext;

        public GDMTreeTests()
        {
            TestUtils.InitGEDCOMProviderTest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        private void OnTreeChange(object sender, EventArgs e) {}
        private void OnTreeChanging(object sender, EventArgs e) {}

        [Test]
        public void Test_Common()
        {
            GDMTree tree = new GDMTree();
            Assert.IsNotNull(tree);

            Assert.IsNotNull(tree.GetSubmitter());

            // Tests of event handlers
            tree.OnChange += OnTreeChange;
            tree.OnChanging += OnTreeChanging;

            tree.ProgressCallback = null;

            tree.BeginUpdate();
            Assert.IsTrue(tree.IsUpdated());

            GDMRecord rec;

            GDMIndividualRecord iRec = tree.CreateIndividual();
            Assert.IsNotNull(iRec, "CreateIndividual() != null");

            string xref = iRec.XRef;
            rec = tree.FindXRef<GDMRecord>(xref);
            Assert.IsNotNull(rec);
            Assert.AreEqual(xref, rec.XRef);

            string uid = iRec.UID;
            rec = tree.FindUID(uid);
            Assert.IsNotNull(rec);
            Assert.AreEqual(uid, rec.UID);
            Assert.IsNull(tree.FindUID(""));

            //
            GDMFamilyRecord famRec = tree.CreateFamily();
            Assert.IsNotNull(famRec, "CreateFamily() != null");

            //
            GDMNoteRecord noteRec = tree.CreateNote();
            Assert.IsNotNull(noteRec, "CreateNote() != null");

            //
            GDMRepositoryRecord repRec = tree.CreateRepository();
            Assert.IsNotNull(repRec, "CreateRepository() != null");

            //
            GDMSourceRecord srcRec = tree.CreateSource();
            Assert.IsNotNull(srcRec, "CreateSource() != null");

            //
            GDMMultimediaRecord mmRec = tree.CreateMultimedia();
            Assert.IsNotNull(mmRec, "CreateMultimedia() != null");
            
            //

            GDMRecord sbmrRec = tree.AddRecord(new GDMSubmitterRecord(tree));
            Assert.IsNotNull(sbmrRec, "sbmrRec != null");
            tree.NewXRef(sbmrRec);
            string submXRef = sbmrRec.XRef;

            //

            GDMSubmissionRecord submRec = tree.AddRecord(new GDMSubmissionRecord(tree)) as GDMSubmissionRecord;
            Assert.IsNotNull(submRec, "rec1 != null");
            tree.NewXRef(submRec);

            //
            GDMGroupRecord groupRec = tree.CreateGroup();
            Assert.IsNotNull(groupRec, "CreateGroup() != null");

            //
            GDMTaskRecord taskRec = tree.CreateTask();
            Assert.IsNotNull(taskRec, "CreateTask() != null");

            //
            GDMCommunicationRecord commRec = tree.CreateCommunication();
            Assert.IsNotNull(commRec, "CreateCommunication() != null");

            //
            GDMResearchRecord resRec = tree.CreateResearch();
            Assert.IsNotNull(resRec, "CreateResearch() != null");

            //
            GDMLocationRecord locRec = tree.CreateLocation();
            Assert.IsNotNull(locRec, "CreateLocation() != null");

            tree.EndUpdate();
            Assert.IsFalse(tree.IsUpdated());

            tree.OnChange -= OnTreeChange;
            tree.OnChanging -= OnTreeChanging;

            int size = 0;
            var enum1 = tree.GetEnumerator(GDMRecordType.rtNone);
            GDMRecord rec1;
            enum1.Reset();
            while (enum1.MoveNext(out rec1)) {
                size++;
            }
            Assert.AreEqual(14, size);

            for (int i = 0; i < tree.RecordsCount; i++) {
                GDMRecord rec2 = tree[i];
                Assert.IsNotNull(rec2);

                string xref2 = rec2.XRef;
                GDMRecord rec3 = tree.FindXRef<GDMRecord>(xref2);
                Assert.IsNotNull(rec3);
                Assert.AreEqual(rec2, rec3);

                /*string uid = rec2.UID;
                GEDCOMRecord rec4 = tree.FindUID(uid);
                Assert.IsNotNull(rec4);
                Assert.AreEqual(rec2, rec4);*/

                int idx = tree.IndexOf(rec2);
                Assert.AreEqual(i, idx);
            }

            var recordsX = tree.GetRecords<GDMIndividualRecord>();
            Assert.IsNotNull(recordsX);
            Assert.AreEqual(1, recordsX.Count);

            int[] stats = tree.GetRecordStats();
            Assert.IsNotNull(stats);
            Assert.AreEqual(14, stats.Length);

            Assert.IsFalse(tree.IsEmpty);

            Assert.IsFalse(tree.DeleteFamilyRecord(null));
            Assert.IsTrue(tree.DeleteFamilyRecord(famRec));

            Assert.IsFalse(tree.DeleteNoteRecord(null));
            Assert.IsTrue(tree.DeleteNoteRecord(noteRec));

            Assert.IsFalse(tree.DeleteSourceRecord(null));
            Assert.IsTrue(tree.DeleteSourceRecord(srcRec));

            Assert.IsFalse(tree.DeleteGroupRecord(null));
            Assert.IsTrue(tree.DeleteGroupRecord(groupRec));

            Assert.IsFalse(tree.DeleteLocationRecord(null));
            Assert.IsTrue(tree.DeleteLocationRecord(locRec));

            Assert.IsFalse(tree.DeleteResearchRecord(null));
            Assert.IsTrue(tree.DeleteResearchRecord(resRec));

            Assert.IsFalse(tree.DeleteCommunicationRecord(null));
            Assert.IsTrue(tree.DeleteCommunicationRecord(commRec));

            Assert.IsFalse(tree.DeleteTaskRecord(null));
            Assert.IsTrue(tree.DeleteTaskRecord(taskRec));

            Assert.IsFalse(tree.DeleteMediaRecord(null));
            Assert.IsTrue(tree.DeleteMediaRecord(mmRec));

            Assert.IsFalse(tree.DeleteIndividualRecord(null));
            Assert.IsTrue(tree.DeleteIndividualRecord(iRec));

            Assert.IsFalse(tree.DeleteRepositoryRecord(null));
            Assert.IsTrue(tree.DeleteRepositoryRecord(repRec));

            tree.Clear();
            Assert.AreEqual(0, tree.RecordsCount);
            Assert.IsTrue(tree.IsEmpty);

            tree.State = GDMTreeState.osReady;
            Assert.AreEqual(GDMTreeState.osReady, tree.State);


            // Tests of GEDCOMTree.Extract()
            using (GDMTree tree2 = new GDMTree()) {
                GDMIndividualRecord iRec2 = tree.AddRecord(new GDMIndividualRecord(tree2)) as GDMIndividualRecord;
                Assert.IsNotNull(iRec2);
                tree2.NewXRef(iRec2);

                tree2.AddRecord(iRec2);
                int rIdx = tree2.IndexOf(iRec2);
                Assert.IsTrue(rIdx >= 0);
                GDMRecord extractedRec = tree2.Extract(rIdx);
                Assert.AreEqual(iRec2, extractedRec);
                Assert.IsTrue(tree2.IndexOf(iRec2) < 0);
            }
        }
    }
}
