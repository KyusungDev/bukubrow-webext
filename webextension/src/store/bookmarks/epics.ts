import { browser } from 'webextension-polyfill-ts';
import { Maybe, Just, Nothing } from 'purify-ts/Maybe';
import { NonEmptyList } from 'purify-ts/NonEmptyList';
import { ThunkAC } from 'Store';
import {
	setAllStagedBookmarksGroups, deleteStagedBookmarksGroup, deleteStagedBookmarksGroupBookmark,
	setBookmarkEditId, setBookmarkDeleteId, setFocusedBookmarkIndex,
	setAddBookmarkModalDisplay, setEditBookmarkModalDisplay, setDeleteBookmarkModalDisplay,
	setAllBookmarks,
} from 'Store/bookmarks/actions';
import { setPage } from 'Store/user/actions';
import { pushError } from 'Store/notices/epics';
import { getWeightedLimitedFilteredBookmarks, getUnlimitedFilteredBookmarks } from 'Store/selectors';
import { saveBookmarkToNative, updateBookmarkToNative, deleteBookmarkFromNative, getBookmarksFromNative } from 'Comms/native';
import { getStagedBookmarksGroupsFromLocalStorage, saveBookmarksToLocalStorage, getBookmarksFromLocalStorage } from 'Comms/browser';
import { untransform, transform } from 'Modules/bookmarks';
import { Page } from 'Store/user/types';

export const getAndSetCachedBookmarks = (): ThunkAC<Promise<void>> => async (dispatch) => {
	const bookmarksRes = await getBookmarksFromLocalStorage().run();

	// Ensuring it's a non-empty list as the focused bookmark index relies upon it
	bookmarksRes.ifJust((bookmarks: NonEmptyList<LocalBookmark>) => {
		dispatch(setAllBookmarks(bookmarks));
		dispatch(setFocusedBookmarkIndex(Just(0)));
	});
};

export const syncBookmarks = (): ThunkAC<Promise<void>> => async (dispatch) => {
	const res = await getBookmarksFromNative();

	Maybe.fromNullable((res && res.success && res.bookmarks) || null)
		.map(bm => bm.map(transform))
		.caseOf({
			Just: bm => saveBookmarksToLocalStorage(bm).then(() => {
				dispatch(getAndSetCachedBookmarks());
			}),
			Nothing: () => {
				const msg = 'Failed to sync bookmarks.';

				dispatch(pushError(msg));
			},
		});
};

export const openBookmarkAndExit = (
	bmId: LocalBookmark['id'],
	stagedBookmarksGroupId: Maybe<StagedBookmarksGroup['id']> = Nothing,
): ThunkAC => (_, getState) => {
	const { bookmarks: { bookmarks, stagedBookmarksGroups } } = getState();

	stagedBookmarksGroupId
		.caseOf({
			Just: grpId => Maybe.fromNullable(stagedBookmarksGroups.find(grp => grp.id === grpId))
				.chain(grp => Maybe.fromNullable(grp.bookmarks.find(bm => bm.id === bmId))),
			Nothing: () => Maybe.fromNullable(bookmarks.find(bm => bm.id === bmId)),
		})
		.ifJust((bookmark) => {
			browser.tabs.create({ url: bookmark.url });

			window.close();
		});
};

export const openAllFilteredBookmarksAndExit = (): ThunkAC => (_, getState) => {
	const filteredBookmarks = getUnlimitedFilteredBookmarks(getState());

	for (const { url } of filteredBookmarks) {
		browser.tabs.create({ url });
	}

	window.close();
};

export const addAllBookmarksFromStagedGroup = (groupId: StagedBookmarksGroup['id']): ThunkAC<Promise<void>> => async (dispatch, getState) => {
	const { bookmarks: { stagedBookmarksGroups } } = getState();

	const bookmarks = Maybe.fromNullable(stagedBookmarksGroups.find(grp => grp.id === groupId))
		// Remove local ID else bookmarks will be detected as saved by
		// untransform overload
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		.map(grp => grp.bookmarks.map(({ id, ...rest }): LocalBookmarkUnsaved => ({ ...rest })))
		.orDefault([]);

	await dispatch(addManyBookmarks(bookmarks));
	dispatch(deleteStagedBookmarksGroup(groupId));
};

export const deleteStagedBookmarksGroupBookmarkOrEntireGroup = (
	grpId: StagedBookmarksGroup['id'],
	bmId: LocalBookmark['id'],
): ThunkAC => (dispatch, getState) => {
	const { bookmarks: { stagedBookmarksGroups } } = getState();

	const grp = stagedBookmarksGroups.find(g => g.id === grpId);
	if (!grp) return;

	if (grp.bookmarks.length === 1) {
		// If deleting last bookmark in group, delete entire group and return to
		// groups list
		dispatch(deleteStagedBookmarksGroup(grpId));
		dispatch(setPage(Page.StagedGroupsList));
	} else {
		// Else delete the bookmark leaving group intact
		dispatch(deleteStagedBookmarksGroupBookmark(grpId, bmId));
	}
};

export const syncStagedBookmarksGroups = (): ThunkAC<Promise<void>> => async (dispatch) => {
	const stagedBookmarksGroups = await getStagedBookmarksGroupsFromLocalStorage().run().then(res => res.orDefault([]));

	dispatch(setAllStagedBookmarksGroups(stagedBookmarksGroups));
};

export const addBookmark = (bookmark: LocalBookmarkUnsaved): ThunkAC<Promise<void>> => async (dispatch) => {
	await saveBookmarkToNative(untransform(bookmark));
	dispatch(syncBookmarks());

	dispatch(setAddBookmarkModalDisplay(false));
};

export const addManyBookmarks = (bookmarks: LocalBookmarkUnsaved[]): ThunkAC<Promise<void>> => async (dispatch) => {
	for (const bookmark of bookmarks) {
		await dispatch(addBookmark(bookmark));
	}
};

export const updateBookmark = (bookmark: LocalBookmark): ThunkAC<Promise<void>> => async (dispatch) => {
	await updateBookmarkToNative(untransform(bookmark));
	dispatch(syncBookmarks());

	dispatch(setEditBookmarkModalDisplay(false));
};

export const deleteBookmark = (): ThunkAC<Promise<void>> => async (dispatch, getState) => {
	const { bookmarkDeleteId } = getState().bookmarks;

	bookmarkDeleteId.ifJust(async (bookmarkId) => {
		await deleteBookmarkFromNative(bookmarkId);
		dispatch(syncBookmarks());

		dispatch(setDeleteBookmarkModalDisplay(false));
	});
};

export const initiateBookmarkEdit = (id: LocalBookmark['id']): ThunkAC => (dispatch) => {
	dispatch(setBookmarkEditId(Just(id)));
	dispatch(setEditBookmarkModalDisplay(true));
};

export const initiateBookmarkDeletion = (id: LocalBookmark['id']): ThunkAC => (dispatch) => {
	dispatch(setBookmarkDeleteId(Just(id)));
	dispatch(setDeleteBookmarkModalDisplay(true));
};

export const attemptFocusedBookmarkIndexIncrement = (): ThunkAC<boolean> => (dispatch, getState) => {
	const state = getState();
	const filteredBookmarks = getWeightedLimitedFilteredBookmarks(state);
	const focusedBookmarkIndexMaybe = state.bookmarks.focusedBookmarkIndex;

	return focusedBookmarkIndexMaybe
		.chain(fbmi => fbmi === filteredBookmarks.length - 1 ? Nothing : Just(fbmi + 1))
		.ifJust((fbmi) => {
			dispatch(setFocusedBookmarkIndex(Just(fbmi)));
		})
		.isJust();
};

export const attemptFocusedBookmarkIndexDecrement = (): ThunkAC<boolean> => (dispatch, getState) => {
	const { bookmarks: { focusedBookmarkIndex: focusedBookmarkIndexMaybe } } = getState();

	return focusedBookmarkIndexMaybe
		.chain(fbmi => fbmi === 0 ? Nothing : Just(fbmi - 1))
		.ifJust((fbmi) => {
			dispatch(setFocusedBookmarkIndex(Just(fbmi)));
		})
		.isJust();
};
