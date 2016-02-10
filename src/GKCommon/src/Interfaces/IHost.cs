using System.Windows.Forms;
using GKCore.Types;

namespace GKCore.Interfaces
{
	public interface IHost : ILocalization
    {
		INamesTable NamesTable { get; }

		IBaseWindow GetCurrentFile(bool extMode = false);
    	IWorkWindow GetWorkWindow();

		IBaseWindow CreateBase(string fileName);
		IBaseWindow FindBase(string fileName);
    	void BaseChanged(IBaseWindow aBase);
    	void BaseClosed(IBaseWindow aBase);
    	void NotifyRecord(IBaseWindow aBase, object record, RecordAction action);

    	string GetAppDataPath();

        void LogWrite(string msg);

        bool IsWidgetActive(IWidget widget);
        void WidgetShow(IWidget widget);
        void WidgetClose(IWidget widget);

        void ShowMDI(Form form);

		ILangMan CreateLangMan(object sender);
		void LoadLanguage(int langCode);
		void UpdateNavControls();
		void UpdateControls(bool forceDeactivate);
		void ShowHelpTopic(string topic);
		void EnableWindow(Form form, bool value);
	}
}
