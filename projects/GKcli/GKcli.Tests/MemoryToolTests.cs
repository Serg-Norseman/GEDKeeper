using System.Text.Json;
using GKcli.Memory;
using NUnit.Framework;

namespace GKcli.Tests;

[TestFixture]
public class MemoryToolTests : MCPToolTests
{
    public MemoryToolTests() : base()
    {
    }

    #region Context Tools Tests

    [Test]
    public void Test_GetContextSummaryTool_ExecuteTool()
    {
        // Arrange
        var tool = new GetContextSummaryTool();
        string sessionId = "test-session-id";
        string jsonString = $"{{ \"session_id\": \"{sessionId}\" }}";
        using var doc = JsonDocument.Parse(jsonString);
        JsonElement args = doc.RootElement;

        // Act
        var result = tool.ExecuteTool(fContext, args);

        // Assert
        Assert.IsNotNull(result);
        Assert.IsTrue(result.Count > 0);
        Assert.IsNotEmpty(result[0].Text);
    }

    [Test]
    public void Test_GetContextSummaryTool_ExecuteTool_MissingSessionId_ThrowsException()
    {
        // Arrange
        var tool = new GetContextSummaryTool();
        string jsonString = "{}";
        using var doc = JsonDocument.Parse(jsonString);
        JsonElement args = doc.RootElement;

        // Act & Assert
        Assert.Throws<ArgumentException>(() => tool.ExecuteTool(fContext, args));
    }

    [Test]
    public void Test_SaveChatMilestoneTool_ExecuteTool()
    {
        // Arrange
        var tool = new SaveChatMilestoneTool();
        string sessionId = "test-session-id";
        string userLine = "Hello, how can you help me?";
        string assistantLine = "I can help you with genealogical research.";
        string jsonString = $"{{ \"session_id\": \"{sessionId}\", \"user_line\": \"{userLine}\", \"assistant_line\": \"{assistantLine}\" }}";
        using var doc = JsonDocument.Parse(jsonString);
        JsonElement args = doc.RootElement;

        // Act
        var result = tool.ExecuteTool(fContext, args);

        // Assert
        Assert.IsNotNull(result);
        Assert.IsTrue(result.Count > 0);
        //Assert.IsTrue(result[0].Text.Contains("✅'], ['Test]
    }

    public void Test_SaveChatMilestoneTool_ExecuteTool_MissingRequiredParameters_ThrowsException()
    {
        // Arrange
        var tool = new SaveChatMilestoneTool();
        string jsonString = "{}";
        using var doc = JsonDocument.Parse(jsonString);
        JsonElement args = doc.RootElement;

        // Act & Assert
        Assert.Throws<ArgumentException>(() => tool.ExecuteTool(fContext, args));
    }

    #endregion

    #region Memory Tools Tests

    [Test]
    public void Test_StoreFactTool_ExecuteTool()
    {
        // Arrange
        var tool = new StoreFactTool();
        string fact = "John Smith was born in 1850";
        string jsonString = $"{{ \"fact\": \"{fact}\" }}";
        using var doc = JsonDocument.Parse(jsonString);
        JsonElement args = doc.RootElement;

        // Act
        var result = tool.ExecuteTool(fContext, args);

        // Assert
        Assert.IsNotNull(result);
        Assert.IsTrue(result.Count > 0);
        //Assert.IsTrue(result[0].Text.Contains("✅'], ['Test]
    }

    public void Test_StoreFactTool_ExecuteTool_MissingFact_ThrowsException()
    {
        // Arrange
        var tool = new StoreFactTool();
        string jsonString = "{}";
        using var doc = JsonDocument.Parse(jsonString);
        JsonElement args = doc.RootElement;

        // Act & Assert
        Assert.Throws<ArgumentException>(() => tool.ExecuteTool(fContext, args));
    }

    [Test]
    public void Test_SearchMemoryTool_ExecuteTool()
    {
        // Arrange
        var tool = new SearchMemoryTool();
        string query = "genealogical research";
        string jsonString = $"{{ \"query\": \"{query}\" }}";
        using var doc = JsonDocument.Parse(jsonString);
        JsonElement args = doc.RootElement;

        // Act
        var result = tool.ExecuteTool(fContext, args);

        // Assert
        Assert.IsNotNull(result);
        Assert.IsTrue(result.Count > 0);
    }

    [Test]
    public void Test_SearchMemoryTool_ExecuteTool_WithTopK()
    {
        // Arrange
        var tool = new SearchMemoryTool();
        string query = "genealogical research";
        int topK = 3;
        string jsonString = $"{{ \"query\": \"{query}\", \"top_k\": {topK} }}";
        using var doc = JsonDocument.Parse(jsonString);
        JsonElement args = doc.RootElement;

        // Act
        var result = tool.ExecuteTool(fContext, args);

        // Assert
        Assert.IsNotNull(result);
        Assert.IsTrue(result.Count > 0);
    }

    [Test]
    public void Test_SearchMemoryTool_ExecuteTool_MissingQuery_ThrowsException()
    {
        // Arrange
        var tool = new SearchMemoryTool();
        string jsonString = "{}";
        using var doc = JsonDocument.Parse(jsonString);
        JsonElement args = doc.RootElement;

        // Act & Assert
        Assert.Throws<ArgumentException>(() => tool.ExecuteTool(fContext, args));
    }

    #endregion
}
