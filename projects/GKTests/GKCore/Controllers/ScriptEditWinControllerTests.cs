/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Design.Views;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class ScriptEditWinControllerTests : ControllerTest
    {
        [Test]
        public void Test_ScriptEditWinController()
        {
            IScriptEditWin view = CreateMockView();
            var controller = new ScriptEditWinController(view);
            controller.Init(fBaseWin);

            controller.NewScript();

            view.ScriptText.Text = ("gk_print(\"Hello\")");
            controller.RunScript();

            view.ScriptText.Text = ("R = gt_get_records_count()");
            controller.RunScript();

            view.ScriptText.Text = ("R = gt_get_record(0); rt = gt_get_record_type(R); " +
                                "xref = gt_get_record_xref(R); uid = gt_get_record_uid(R);" +
                                "isf = gt_record_is_filtered(R); tn = gt_get_record_type_name(rt);" +
                                "num = gt_get_records_count();");
            controller.RunScript();

            view.ScriptText.Text = ("gk_progress_init(1, \"Hello\"); gk_progress_step(); gk_progress_done(); gk_update_view()");
            controller.RunScript();

            view.ScriptText.Text = ("x = gk_strpos(\"test\", \"alpha test\");");
            controller.RunScript();

            view.ScriptText.Text = ("indi = gt_create_person(\"Ivan\", \"Ivanovich\", \"Ivanov\", \"M\");" +
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
            controller.RunScript();

            view.ScriptText.Text = ("indi = gt_create_person(\"John\", \"\", \"Smith\", \"M\");" +
                                "evt = gt_create_event(indi, \"FACT\");" +
                                "gt_set_event_date(evt, \"08 MAR 1990\");" +
                                "gt_set_event_place(evt, \"sample place\");" +
                                "gt_set_event_value(evt, \"sample value\");" +
                                "ed = gt_get_event_date(evt);" +
                                "en = gt_get_event_name(evt);" +
                                "ep = gt_get_event_place(evt);" +
                                "ev = gt_get_event_value(evt);" +
                                "ey = gt_get_event_year(evt);");
            controller.RunScript();

            view.ScriptText.Text = ("fam = gt_create_family(); evt = gt_create_event(fam, \"MARR\");" +
                                "R = gt_get_record(0); gt_bind_family_spouse(fam, R); " +
                                "R2 = gt_get_record(1); gt_bind_family_child(fam, R2); " +
                                "chNum = gt_get_family_childs_count(fam); chl = gt_get_family_child(fam, 0);" +
                                "h = gt_get_family_husband(fam); w = gt_get_family_wife(fam);" +
                                "spNum = gt_get_person_spouses_count(R);" +
                                "fam2 = gt_get_person_spouse_family(R, 0);");
            controller.RunScript();

            view.ScriptText.Text = ("note = gt_create_note(); gt_add_note_text(note, \"test\");" +
                                "R = gt_get_record(0); gt_bind_record_note(R, note); " +
                                "ntNum = gt_get_record_notes_count(R);");
            controller.RunScript();

            view.ScriptText.Text = ("src = gt_create_source(\"source\");" +
                                "R = gt_get_record(0); gt_bind_record_source(R, src, \"p1\", 1); " +
                                "src = gt_find_source(\"source\");");
            controller.RunScript();

            view.ScriptText.Text = ("grp = gt_create_group(\"group\");" +
                                "R = gt_get_record(0); gt_bind_group_member(grp, R); " +
                                "gname = gt_get_group_name(grp);" +
                                "gNum = gt_get_person_groups_count(R); grp1 = gt_get_person_group(R, 0);" +
                                "gt_delete_record(grp);");
            controller.RunScript();

            view.ScriptText.Text = ("x = gt_get_location_usages(loc);"); // -1
            controller.RunScript();

            view.ScriptText.Text = ("con = ado_open(\"test\"); qr = ado_query_open(con, \"select * from X\"); " +
                                "ado_query_first(con); ado_query_prev(con);" +
                                "ado_query_next(con); ado_query_last(con);" +
                                "x = ado_get_query_field(con, \"field\");" +
                                "ado_query_close(qr); ado_dump(con);  ado_close(con);");
            controller.RunScript();

            //StdDialogsStub.SetOpenedFile(...);
            view.ScriptText.Text = ("file = gk_select_file();");
            controller.RunScript();

            //StdDialogsStub.SetOpenedFile(...);
            view.ScriptText.Text = ("R = gt_select_record(rtIndividual);");
            controller.RunScript();

            /*SetModalFormHandler(fFormTest, OpenFile_Cancel_Handler);
            ClickToolStripButton("tbLoadScript", form);

            SetModalFormHandler(fFormTest, SaveFile_Cancel_Handler);
            ClickToolStripButton("tbSaveScript", form);*/
        }

        private static IScriptEditWin CreateMockView()
        {
            var view = Substitute.For<IScriptEditWin>();
            SubstituteControl<IToolItem>(view, "tbNewScript");
            SubstituteControl<IToolItem>(view, "tbLoadScript");
            SubstituteControl<IToolItem>(view, "tbSaveScript");
            SubstituteControl<IToolItem>(view, "tbRun");

            view.ScriptText.Returns(Substitute.For<ITextBox>());
            view.DebugOutput.Returns(Substitute.For<ITextBox>());
            return view;
        }
    }
}
