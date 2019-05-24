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

#if !__MonoCS__

using System;
using System.Windows.Forms;
using GDModel;
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using GKUI.Forms;
using GKUI.Providers;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class ScriptEditWinTests : CustomWindowTest
    {

        #region Handlers for external tests

        public static void ScriptEditWin_Handler(string name, IntPtr ptr, Form form)
        {
            ScriptEditWin scriptWin = form as ScriptEditWin;
            Assert.AreEqual("unknown.lua", scriptWin.FileName);

            var txtScriptText = new TextBoxTester("txtScriptText");

            txtScriptText.Enter("gk_print(\"Hello\")");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("R = gt_get_records_count()");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("R = gt_get_record(0); rt = gt_get_record_type(R); "+
                                "xref = gt_get_record_xref(R); uid = gt_get_record_uid(R);"+
                                "isf = gt_record_is_filtered(R); tn = gt_get_record_type_name(rt);"+
                                "num = gt_get_records_count();");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("gk_progress_init(1, \"Hello\"); gk_progress_step(); gk_progress_done(); gk_update_view()");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("x = gk_strpos(\"test\", \"alpha test\");");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("indi = gt_create_person(\"Ivan\", \"Ivanovich\", \"Ivanov\", \"M\");"+
                                "gt_set_person_sex(indi, \"M\"); name = gt_get_person_name(indi);" +
                                "gt_add_person_association(indi, \"rel\", indi);" +
                                "assoNum = gt_get_person_associations_count(indi);" +
                                "asso = gt_get_person_association(indi, 0);" +
                                "gt_delete_person_association(indi, 0);" +
                                "evtNum = gt_get_person_events_count(indi);" +
                                "evt = gt_get_person_event(indi, 0);" +
                                "gt_delete_person_event(indi, 0);" +
                                "parentsFam = gt_get_person_parents_family(indi);" +
                                "sx = gt_get_person_sex(indi);" +
                                "evt2 = gt_get_person_event_ex(indi, \"BIRT\");");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("indi = gt_create_person(\"John\", \"\", \"Smith\", \"M\");" +
                                "evt = gt_create_event(indi, \"FACT\");" +
                                "gt_set_event_date(evt, \"08 MAR 1990\");" +
                                "gt_set_event_place(evt, \"sample place\");" +
                                "gt_set_event_value(evt, \"sample value\");" +
                                "ed = gt_get_event_date(evt);" +
                                "en = gt_get_event_name(evt);" +
                                "ep = gt_get_event_place(evt);" +
                                "ev = gt_get_event_value(evt);" +
                                "ey = gt_get_event_year(evt);");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("fam = gt_create_family(); evt = gt_create_event(fam, \"MARR\");" +
                                "R = gt_get_record(0); gt_bind_family_spouse(fam, R); " +
                                "R2 = gt_get_record(1); gt_bind_family_child(fam, R2); " +
                                "chNum = gt_get_family_childs_count(fam); chl = gt_get_family_child(fam, 0);" +
                                "h = gt_get_family_husband(fam); w = gt_get_family_wife(fam);" +
                                "spNum = gt_get_person_spouses_count(R);" +
                                "fam2 = gt_get_person_spouse_family(R, 0);");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("note = gt_create_note(); gt_add_note_text(note, \"test\");" +
                                "R = gt_get_record(0); gt_bind_record_note(R, note); " +
                                "ntNum = gt_get_record_notes_count(R);");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("src = gt_create_source(\"source\");" +
                                "R = gt_get_record(0); gt_bind_record_source(R, src, \"p1\", 1); " +
                                "src = gt_find_source(\"source\");");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("grp = gt_create_group(\"group\");" +
                                "R = gt_get_record(0); gt_bind_group_member(grp, R); " +
                                "gname = gt_get_group_name(grp);" +
                                "gNum = gt_get_person_groups_count(R); grp1 = gt_get_person_group(R, 0);" +
                                "gt_delete_record(grp);");
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("x = gt_get_location_usages(loc);"); // -1
            ClickToolStripButton("tbRun", form);

            txtScriptText.Enter("con = ado_open(\"test\"); qr = ado_query_open(con, \"select * from X\"); "+
                                "ado_query_first(con); ado_query_prev(con);"+
                                "ado_query_next(con); ado_query_last(con);"+
                                "x = ado_get_query_field(con, \"field\");"+
                                "ado_query_close(qr); ado_dump(con);  ado_close(con);");
            ClickToolStripButton("tbRun", form);

            SetModalFormHandler(fFormTest, OpenFile_Cancel_Handler);
            txtScriptText.Enter("file = gk_select_file();");
            ClickToolStripButton("tbRun", form);

            SetModalFormHandler(fFormTest, Dialog_Cancel_Handler);
            txtScriptText.Enter("R = gt_select_record(rtIndividual);");
            ClickToolStripButton("tbRun", form);

            SetModalFormHandler(fFormTest, OpenFile_Cancel_Handler);
            ClickToolStripButton("tbLoadScript", form);

            SetModalFormHandler(fFormTest, SaveFile_Cancel_Handler);
            ClickToolStripButton("tbSaveScript", form);

            SetModalFormHandler(fFormTest, MessageBox_NoHandler);
            KeyDownForm(form.Name, Keys.Escape);
            form.Dispose();
        }

        #endregion
    }
}

#endif
