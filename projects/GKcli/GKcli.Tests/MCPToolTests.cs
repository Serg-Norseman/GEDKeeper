using GKcli.Features;
using GKCore;
using GKCore.Locales;

namespace GKcli.Tests;

public class MCPToolTests
{
    protected readonly BaseContext fContext;

    static MCPToolTests()
    {
        LangMan.DefInit();
        MCPController.InitFeatures(pureMode: false, tdeMode: true, ragMode: true);
    }

    public MCPToolTests()
    {
        fContext = new BaseContext(null);
    }
}
