typedef void *HWND;

typedef struct _RPC_ASYNC_STATE {
union {
    struct {
HWND hWnd;
int Msg;
    } HWND;
} u;
} RPC_ASYNC_STATE;
