%% 
%% FireFork Remote control GUI module
-module(firefork_control_panel).

-behaviour(wx_object).  

%% Client API
% TODO: remote interface for controls 

%% wx_object callbacks
-export([start/0, start/1, start_link/0, start_link/1, format/3]).
-export([init/1, terminate/2,  code_change/3, handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("../include/wx.hrl").

% IDs for gui elements
-define(menuID_FILE_QUIT,          ?wxID_EXIT).
-define(menuID_FILE_CLEAR_LOG,     100).
-define(menuID_FILE_LOAD1,         110).
-define(menuID_RUN_NORMAL,         400).
-define(menuID_RUN_CHECK,          401).
-define(menuID_RUN_RADIO_1,        402).
-define(menuID_RUN_RADIO_2,        403).
-define(menuID_RUN_RADIO_3,        404).

-define(btnID_TEST,    10).
-define(btnID_START,   11).
-define(btnID_STOP,    12).

-define(fileID_SCRIPT, 21).
-define(fileID_AUDIO,  22).


-define(menuID_HELP_ABOUT,         ?wxID_ABOUT).
-define(wID_LOG_TEXT_CTRL, 3000).

-record(state, {
    win,         % master wx frame
    log,         % event log copmponent    
    audio_file,  % audio file with path
    script_file, % script file with path
    vsn
    }).


start() ->
    start([]).

start(Debug) ->
    Frame = wx_object:start(?MODULE, Debug, []),
    Pid = wx_object:get_pid(Frame),
    {ok, Pid}.

start_link() ->
    start_link([]).

start_link(Debug) ->
    Frame = wx_object:start_link(?MODULE, Debug, []),
    Pid = wx_object:get_pid(Frame),
    {ok, Pid}.

format(Config,Str,Args) ->
    Log = proplists:get_value(log, Config),
    wxTextCtrl:appendText(Log, io_lib:format(Str, Args)),
    ok.


%%-----------------------------------------------------------------------------
%% initialization
%%-----------------------------------------------------------------------------
init(Options) ->
try
    wx:new(Options),
    process_flag(trap_exit, true),

    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "FireFork Control Panel", [{size, {600,600}}]),

    % init menu
    FileMenu = create_file_menu(),
    RunMenu  = create_run_menu(),
    HelpMenu = create_help_menu(),
    MenuBar    = wxMenuBar:new(),
    wxMenuBar:append(MenuBar, FileMenu, "&File"),
    wxMenuBar:append(MenuBar, RunMenu,  "&Run"),
    wxMenuBar:append(MenuBar, HelpMenu, "&Help"),
    wxFrame:setMenuBar(Frame, MenuBar),

    % status bar
    wxFrame:createStatusBar(Frame,[]),
    ok = wxFrame:setStatusText(Frame, "Under development...",[]),

    % set main layout
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    wxFrame:setSizer(Frame, MainSz),
    MainSzOptions = [{border, 4},{flag, ?wxALL bor ?wxEXPAND}],

    FilesPanel = wxPanel:new(Frame, [{size, {400, 100}}]),
    wxSizer:add(MainSz, FilesPanel, [{proportion, 3}] ++ MainSzOptions), 

    CtrlPanel = wxPanel:new(Frame, [{size, {400, 40}}]),
    wxSizer:add(MainSz, CtrlPanel, [{proportion, 2}] ++ MainSzOptions), 

    SliderPanel = wxPanel:new(Frame, [{size, {400, 50}}]),
    wxSizer:add(MainSz, SliderPanel,[{proportion, 2}] ++ MainSzOptions), 

    EventPanel = wxPanel:new(Frame, [{size, {400, 200}}]),
    wxSizer:add(MainSz, EventPanel, [{proportion, 3}] ++ MainSzOptions),

    % select files panel
    FilesSz = wxStaticBoxSizer:new(?wxVERTICAL, FilesPanel, [{label, "Select files"}]),
    wxPanel:setSizer(FilesPanel, FilesSz),
    
    ScriptFilePicker = wxFilePickerCtrl:new(FilesPanel, 21, [{path, "/"}]),
    AudioFilePicker  = wxFilePickerCtrl:new(FilesPanel, 22, [{path, "/"}]),

    wxFilePickerCtrl:connect(ScriptFilePicker, command_filepicker_changed, []),
    wxFilePickerCtrl:connect(AudioFilePicker, command_filepicker_changed, []),

    PickerOptions = [{border, 4},{flag, ?wxALL bor ?wxEXPAND}],
    wxSizer:add(FilesSz, ScriptFilePicker, PickerOptions),
    wxSizer:add(FilesSz, AudioFilePicker, PickerOptions),


    % control panel
    CtrlSz = wxStaticBoxSizer:new(?wxHORIZONTAL, CtrlPanel, [{label, "Remote controls"}]),
    wxPanel:setSizer(CtrlPanel, CtrlSz),

    B10 = wxButton:new(CtrlPanel, 10, [{label,"Test"}]),
    wxButton:setToolTip(B10, "Test connectivity, and file checksums"),

    B11 = wxButton:new(CtrlPanel, 11, [{label,"Start"}]),
    wxButton:setToolTip(B11, "Start launch script and audio"),

    B12 = wxButton:new(CtrlPanel, 12, [{label,"Stop"}]),
    wxButton:setToolTip(B12, "Send stop command to player"),

    wxSizer:add(CtrlSz, B10, [{proportion, 1}, {border, 4}, {flag, ?wxALL}]),    
    wxSizer:add(CtrlSz, B11, [{proportion, 1}, {border, 4}, {flag, ?wxALL}]),    
    wxSizer:add(CtrlSz, B12, [{proportion, 1}, {border, 4}, {flag, ?wxALL}]),    

    % progress bar
    SliderSz = wxStaticBoxSizer:new(?wxVERTICAL, SliderPanel, [{label, "Progress bar"}]),
    wxPanel:setSizer(SliderPanel, SliderSz),

    Min = 0,
    Max = 100,
    StartValue = 0,
    %% Horizontal slider with labels
    Slider = wxSlider:new(SliderPanel, 1, StartValue, Min, Max,
                [{style, ?wxSL_HORIZONTAL bor ?wxSL_LABELS}]),

    wxSizer:add(SliderSz, Slider, [{flag, ?wxEXPAND}]),


    % events area 
    EventSz = wxStaticBoxSizer:new(?wxVERTICAL, EventPanel, [{label, "Events log"}]),
    wxPanel:setSizer(EventPanel, EventSz),
    EvCtrl = 
        wxTextCtrl:new(
            EventPanel, 
            ?wID_LOG_TEXT_CTRL, 
            [{style, ?wxTE_DONTWRAP bor ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
    wxTextCtrl:appendText(EvCtrl, "FireFork app started..\n"),    

    wxSizer:add(EventSz, EvCtrl, [{proportion, 1}, {border, 4}, {flag, ?wxEXPAND}]),

    % wx events
    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_menu_selected),
    ok = wxWindow:connect(CtrlPanel, command_button_clicked),

    % set defaults
    String = "~nShort instructions: ~n"
            " - Load prepared firescript (.csv) and audio (.mp3) files.~n"
            " - Test conectivity with Ignition module~n"
            " - Launch fireworks and run! ~n~n"
            "---------------------------------~n",

    logMessage(Frame, String),

    % show
    wxFrame:show(Frame),
    State = #state{win=Frame, log=EvCtrl},

    {Frame, State}
catch
    ErrClass:ErrReason ->
        {error, ErrClass, ErrReason, erlang:get_stacktrace()}
end.

%%-----------------------------------------------------------------------------
%% helper init functions
%%-----------------------------------------------------------------------------
create_file_menu() ->
    FileMenu  = wxMenu:new(),

    wxMenu:append(FileMenu, wxMenuItem:new([
            {id,        ?menuID_FILE_LOAD1},
            {text,      "&Load audio file"},
            {help,  "todo"}
            ])),
    ClearLogItem = wxMenuItem:new([
            {id,    ?menuID_FILE_CLEAR_LOG},
            {text,  "Clear &log\tCtrl-L"}   %% note mnemonic and accelerator
            ]),

    wxMenu:append(FileMenu, ClearLogItem ),
    wxMenu:appendSeparator(FileMenu),
    wxMenu:append(FileMenu, wxMenuItem:new([
            {id, ?menuID_FILE_QUIT} %,
            %{text, "E&xit\tAlt-X"}
            ])),
    FileMenu.

%%
%%
create_run_menu() ->
    RunMenu   = wxMenu:new(),
    wxMenu:append(RunMenu, wxMenuItem:new([
            {id,    ?menuID_RUN_NORMAL},
            {text,  "&Normal submenu item"},
            {help,  "Disabled submenu item"}
            ])),
    wxMenu:appendSeparator(RunMenu), %% --------------------------
    %% note different way of adding check menu item
    wxMenu:appendCheckItem(RunMenu, ?menuID_RUN_CHECK,    "&Check item"),
    wxMenu:appendSeparator(RunMenu), %% --------------------------
    wxMenu:appendRadioItem(RunMenu, ?menuID_RUN_RADIO_1,  "Radio item &1"),
    wxMenu:appendRadioItem(RunMenu, ?menuID_RUN_RADIO_2,  "Radio item &2"),
    wxMenu:appendRadioItem(RunMenu, ?menuID_RUN_RADIO_3,  "Radio item &3"),
    RunMenu.

%%
%%
create_help_menu() ->
    HelpMenu =  wxMenu:new(),
    wxMenu:append(HelpMenu, wxMenuItem:new([
            {id,    ?wxID_HELP},
            {text,  "&Help\tF1"},
            {help,  "More info"}
            ])),
    wxMenu:append(HelpMenu, wxMenuItem:new([
            {id,    ?menuID_HELP_ABOUT},
            %{text,  "&About\tF1"},
            {help,  "About menu"}
            ])),
    HelpMenu.


%%-----------------------------------------------------------------------------
%% Callbacks
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Handled as in normal gen_server callbacks
handle_info({'EXIT',_, wx_deleted}, State) ->
    {noreply,State};
handle_info({'EXIT',_, normal}, State) ->
    {noreply,State};
handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

%%-----------------------------------------------------------------------------
%% Async Events are handled in handle_event as in handle_info
%%
handle_event(#wx{id = Id,
        event = #wxCommand{type = command_menu_selected}},
        State = #state{}) ->
    case Id of
    ?wxID_HELP ->
        wx_misc:launchDefaultBrowser("https://github.com/spawnfest/fire-fork"),
        {noreply, State};
    ?wxID_EXIT ->
        io:format("~p Closing window ~n",[self()]),
        {stop, normal, State};
    _ ->
        State1 = onMenuAction(Id, State),
        {noreply, State1}
    end;

%%
handle_event(#wx{id=Id, 
        event=#wxCommand{type=command_button_clicked}},
        State = #state{win=Frame}) ->
    case Id of
    ?btnID_TEST ->
        logMessage(Frame, "~nTest command executed."),
        firefork_station_intf_stepper:send_check(scenario1, 100), % TODO provide params
        {noreply, State};

    ?btnID_START ->
        logMessage(Frame, "~nStart command executed."),
        firefork_station_intf_stepper:send_start(scenario1, 100, 0), % TODO provide params
        AudioPath = State#state.audio_file,
        % TODO audio should start after ACK received from stepper
        case AudioPath of
            undefined ->
                logMessage(Frame, "~nWarrning audio file not selected!"); 
            _ -> 
                firefork_audio_player:play(AudioPath)
        end, 
        {noreply, State};

    ?btnID_STOP ->
        logMessage(Frame, "~nStop command executed."),
        firefork_station_intf_stepper:send_stop(scenario1, 100), 
        firefork_audio_player:stop(),
        {noreply, State};

    _ ->
        logMessage(Frame, "Unhandled button action (wxId=~p).", [Id]),
        {noreply, State}
    end;

% select file events
handle_event(#wx{id = Id,
        event = #wxFileDirPicker{
            type = command_filepicker_changed,
            path = Path}},
        State = #state{win=Frame}) ->
    case Id of
    ?fileID_SCRIPT ->
        logMessage(Frame, "~nLaunch script selected."),
        State1 = State#state{script_file=Path},
        {noreply, State1};

    ?fileID_AUDIO ->
        logMessage(Frame, "~nAudio file selected."),
        State1 = State#state{audio_file=Path},
        {noreply, State1};

    _ ->
        logMessage(Frame, "Unhandled action (wxId=~p).", [Id]),
        {noreply, State}
    end;

%%
handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
    io:format("~p Closing window ~n",[self()]),
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    {stop, normal, State};


handle_event(Ev,State = #state{win=Frame}) ->
    io:format("~p Got event ~p ~n",[?MODULE, Ev]),
    logMessage(Frame, "~nUnhandled GUI event: ~n  ~p ~n",[Ev]),
    {noreply, State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _) ->
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-----------------------------------------------------------------------------
%%  Handle menu events
%%-----------------------------------------------------------------------------
onMenuAction(?menuID_FILE_CLEAR_LOG, State = #state{win=Frame}) ->
    wxTextCtrl:clear(findLogger(Frame)),
    State;

onMenuAction(?menuID_HELP_ABOUT = Id, State = #state{win=Frame}) ->
    showDialog(Id, Frame),
    State;

onMenuAction(Id, State = #state{win=Frame}) ->
    logMessage(Frame, "unimplemented menu item event (id=~p) ~n", [Id]),
    State.

%%------------------------------------------------------------
%%  Other local functions
%%------------------------------------------------------------

logMessage(Frame, Msg) ->
    logMessage(Frame, Msg, []).

logMessage(Frame, Msg, ArgList) ->
    String  = lists:flatten(io_lib:format(Msg, ArgList)),
    wxTextCtrl:appendText(findLogger(Frame), String).

findLogger(Frame) ->
    LogWin = wxWindow:findWindowById(?wID_LOG_TEXT_CTRL, [{parent, Frame}]),
    wx:typeCast(LogWin, wxTextCtrl).


showDialog(?menuID_HELP_ABOUT,  Frame) ->
    String = lists:flatten(io_lib:format("Welcome to FireFork Remote Control!~n~n"
               "This app is created during erlang SpawnFest 2017. ~n"
               "running under ~p.",
               [wx_misc:getOsDescription()])),
    MessageDialog = wxMessageDialog:new(Frame,
                 String,
                 [{style, ?wxOK bor ?wxICON_INFORMATION},
                  {caption, "About FireFork"}]),
    wxDialog:showModal(MessageDialog),
    wxDialog:destroy(MessageDialog).    