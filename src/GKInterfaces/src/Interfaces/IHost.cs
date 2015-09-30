using System.Windows.Forms;
using GKCore.Types;

namespace GKCore.Interfaces
{
    public interface IHost
    {
		ILangMan CreateLangMan(object sender);
    	IBase GetCurrentFile(bool extMode = false);
    	IWorkWindow GetWorkWindow();

    	void BaseChanged(IBase aBase);
    	void BaseClosed(IBase aBase);
    	void NotifyRecord(IBase aBase, object record, RecordAction action);
    	
    	string GetAppDataPath();

        bool Register(IPlugin plugin);
        void LogWrite(string msg);

        bool IsWidgetActive(IWidget widget);
        void WidgetShow(IWidget widget);
        void WidgetClose(IWidget widget);
        
        void ShowMDI(Form form);
    }
}
