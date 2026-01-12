/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Plugins;
using NSubstitute;

namespace GKTests.Stubs
{
    public class TestPlugin : OrdinaryPlugin
    {
        private ILangMan fLangMan;

        public override string DisplayName { get { return "TestPlugin"; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Common; } }

        public TestPlugin()
        {
            fLangMan = null;
        }

        public TestPlugin(ILangMan langMan)
        {
            fLangMan = (langMan != null) ? langMan : Substitute.For<ILangMan>();
        }

        public override void Execute()
        {
        }
    }
}
