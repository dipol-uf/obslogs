
using System;

namespace DIPolWeb.FetchData;

internal abstract record Condition;

internal abstract record Condition<T> : Condition
{
    public abstract bool Matches(T obj);
}


internal record NumericCondition(int Value) : Condition<int>
{
    public override bool Matches(int obj) => obj == Value;

}

internal record NumericRangeCondition(NumericCondition Lower, NumericCondition Upper) : Condition<int>
{
    public override bool Matches(int obj) => obj >= Lower.Value && obj <= Upper.Value;
}

internal record DateCondition(DateOnly Value) : Condition<DateOnly>
{
    public override bool Matches(DateOnly obj) => obj == Value;
}

internal record DateRangeCondition(DateCondition Lower, DateCondition Upper) : Condition<DateOnly>
{
    public override bool Matches(DateOnly obj) => obj >= Lower.Value && obj <= Upper.Value;
}

internal record CaseInsensitiveTextCondition(string Value) : Condition<string>
{
    public override bool Matches(string obj) => obj.Equals(Value, StringComparison.OrdinalIgnoreCase);
}

internal record AndCondition<T>(Condition<T> Lhs, Condition<T> Rhs) : Condition<T>
{
    public override bool Matches(T obj) => Lhs.Matches(obj) && Rhs.Matches(obj);
}

internal record OrCondition<T>(Condition<T> Lhs, Condition<T> Rhs) : Condition<T>
{
    public override bool Matches(T obj) => Lhs.Matches(obj) || Rhs.Matches(obj);
}
