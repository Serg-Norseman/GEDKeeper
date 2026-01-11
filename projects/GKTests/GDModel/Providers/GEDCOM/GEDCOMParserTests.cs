// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System;
using NUnit.Framework;

namespace GDModel.Providers.GEDCOM
{
    [TestFixture]
    public class GEDCOMParserTests
    {
        [Test]
        public void Test_CtorStrNull()
        {
            Assert.Throws(typeof(ArgumentNullException), () => {
                string data = null;
                new GEDCOMParser(data, false);
            });
        }
    }
}
