// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMRecordTests
    {
        [Test]
        public void Test_GDMRecord()
        {
            GDMRecord obj = new GDMRecord(null);
            obj.Dispose();
        }
    }
}
