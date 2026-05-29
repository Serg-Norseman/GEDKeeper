/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;
using GKTests;
using GKTests.Stubs;
using NSubstitute;

namespace GKCore.Controllers
{
    public abstract class ControllerTest
    {
        protected readonly IBaseWindow fBaseWin;

        protected ControllerTest()
        {
            TestUtils.InitUITest();
            fBaseWin = new BaseWindowStub(true);

            // prevent LanguageSelectDlg modal dialog from showing on first run
            AppHost.Options.InterfaceLang = LangMan.LS_DEF_CODE;

            // required for testing, otherwise the engine will require saving
            // the database (requires path of files for the archive and storage)
            GlobalOptions.Instance.AllowMediaStoreReferences = true;
        }

        protected static IBaseWindow GetBaseSubst()
        {
            var baseWin = Substitute.For<IBaseWindow>();
            var baseContext = new BaseContext(baseWin);
            baseWin.Context.Returns(baseContext);
            return baseWin;
        }

        protected static void SubstituteControl<T>(IView dialog, string ctlName) where T : class, IControl
        {
            TestUtils.SubstituteControl<T>(dialog, ctlName);
        }

        protected static async Task ModifySheetList<T>(ISheetList sheetList, RecordAction action, object itemData) where T : ISheetModel
        {
            await ((T)sheetList.ListModel).Modify(sheetList, new ModifyEventArgs(action, itemData));
        }

        protected static ISheetList SubstituteSheetList(IListSource listSource)
        {
            var sheetList = Substitute.For<ISheetList>();
            var listView = Substitute.For<IListView>();
            sheetList.ListView.Returns(listView);
            listView.ListMan.Returns(listSource);
            return sheetList;
        }
    }
}
