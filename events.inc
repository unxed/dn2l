{ interface block for Events.pas }

function GetCurMSec: Longint;
{` Get system uptime in mSec `}

procedure LongWorkBegin;
{` ��� Win9x �ਮ��� ᭨������ �� ��ଠ�쭮��, �⮡� �� �ମ����
��� ��⥬� (���筮 �� ��� W9x ࠡ�⠥� � ����襭�� �ਮ��⮬,
⠪ ��� ���� �ମ��� � ��� ���������.)
 `}

procedure LongWorkEnd;
{` ��� Win9x ����⠭���������� ����襭�� �ਮ���; �. LongWorkBegin;.
`}

