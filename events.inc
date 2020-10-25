{ interface block for Events.pas }

function GetCurMSec: Longint;
{` Get system uptime in mSec `}

procedure LongWorkBegin;
{` Под Win9x приоритет снижается до нормального, чтобы не тормозить
всю систему (обычно мы под W9x работаем с повышенным приоритетом,
так как иначе тормозит у нас клавиатура.)
 `}

procedure LongWorkEnd;
{` Под Win9x восстанавливается повышенный приоритет; см. LongWorkBegin;.
`}

