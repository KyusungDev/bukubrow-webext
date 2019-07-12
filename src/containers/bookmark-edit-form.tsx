import React, { FC } from 'react';
import { useSelector, useDispatch } from 'Store';
import { getBookmarkToEdit } from 'Store/selectors';
import { updateBookmark } from 'Store/bookmarks/epics';
import BookmarkForm from 'Components/bookmark-form';

const BookmarkEditForm: FC = () => {
	const bookmarkToEdit = useSelector(getBookmarkToEdit);
	const dispatch = useDispatch();

	return (
		<BookmarkForm
			bookmark={bookmarkToEdit}
			// TODO assertion
			onSubmit={(bm: LocalBookmark) => dispatch(updateBookmark(bm))}
		/>
	);
};

export default BookmarkEditForm;
