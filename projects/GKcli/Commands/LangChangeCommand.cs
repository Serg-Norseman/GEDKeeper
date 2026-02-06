/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Locales;
using GKCore.Options;
using Sharprompt;

namespace GKUI.Commands;

internal class LangChangeCommand : BaseCommand
{
    public LangChangeCommand() : base("language", LangMan.LS(LSID.Language), CommandCategory.Service)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var langs = AppHost.Options.Languages;
        if (langs.Count > 0) {
            var selectedLang = Prompt.Select("Select a language", langs, pageSize: 10,
                textSelector: (LangRecord r) => { return r.Name; });

            CommandController.WriteLine(string.Format("Selected: {0}", selectedLang.FileName));
            AppHost.Instance.LoadLanguage(selectedLang.Code, false);
        }
    }
}
